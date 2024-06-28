parser grammar GQLParser;

options { tokenVocab = GQLLexer; }

gqlRequest
   : gqlProgram SEMICOLON? EOF
   ;

GQLProgram
   : programActivity sessionCloseCommand?
   | sessionCloseCommand
   ;

programActivity
   : sessionActivity
   | transactionActivity
   ;

sessionActivity
   : sessionResetCommand+
   | sessionSetCommand+ sessionResetCommand*
   ;

transactionActivity
   : startTransactionCommand (procedureSpecification endTransactionCommand?)?
   | procedureSpecification endTransactionCommand?
   | endTransactionCommand
   ;

endTransactionCommand
   : rollbackCommand
   | commitCommand
   ;

sessionSetCommand
   : SESSION SET (sessionSetSchemaClause | sessionSetGraphClause | sessionSetTimeZoneClause | sessionSetParameterClause)
   ;

sessionSetSchemaClause
   : SCHEMA schemaReference
   ;

sessionSetGraphClause
   : PROPERTY? GRAPH graphExpression
   ;

sessionSetTimeZoneClause
   : TIME ZONE setTimeZoneValue
   ;

setTimeZoneValue
   : TIME_ZONE_STRING
   ;

sessionSetParameterClause
   : sessionSetGraphParameterClause
   | sessionSetBindingTableParameterClause
   | sessionSetValueParameterClause
   ;

sessionSetGraphParameterClause
   : PROPERTY? GRAPH sessionSetParameterName optTypedGraphInitializer
   ;

sessionSetBindingTableParameterClause
   : BINDING? TABLE sessionSetParameterName optTypedBindingTableInitializer
   ;

sessionSetValueParameterClause
   : VALUE sessionSetParameterName optTypedValueInitializer
   ;

sessionSetParameterName
   : (IF NOT EXISTS)? sessionParameterSpecification
   ;

sessionResetCommand
   : SESSION RESET sessionResetArguments?
   ;

sessionResetArguments
   : ALL? (PARAMETERS | CHARACTERISTICS)
   | SCHEMA
   | PROPERTY? GRAPH
   | TIME ZONE
   | PARAMETER? sessionParameterSpecification
   ;

sessionCloseCommand
   : SESSION CLOSE
   ;

sessionParameterSpecification
   : GENERAL_PARAMETER_REFERENCE
   ;

startTransactionCommand
   : START TRANSACTION transactionCharacteristics?
   ;

transactionCharacteristics
   : transactionMode (COMMA transactionMode)*
   ;

transactionMode
   : transactionAccessMode
   | implementationDefinedAccessMode
   ;

transactionAccessMode
   : READ ONLY
   | READ WRITE
   ;

implementationDefinedAccessMode
   : 
   ;

rollbackCommand
   : ROLLBACK
   ;

commitCommand
   : COMMIT
   ;

nestedProcedureSpecification
   : LEFT_BRACE procedureSpecification RIGHT_BRACE
   ;

procedureSpecification
   : catalogModifyingProcedureSpecification
   | dataModifyingProcedureSpecification
   | querySpecification
   ;

catalogModifyingProcedureSpecification
   : procedureBody
   ;

nestedDataModifyingProcedureSpecification
   : LEFT_BRACE dataModifyingProcedureSpecification RIGHT_BRACE
   ;

dataModifyingProcedureSpecification
   : procedureBody
   ;

nestedQuerySpecification
   : LEFT_BRACE querySpecification RIGHT_BRACE
   ;

querySpecification
   : procedureBody
   ;

procedureBody
   : atSchemaClause? bindingVariableDefinitionBlock? statementBlock
   ;

bindingVariableDefinitionBlock
   : bindingVariableDefinition+
   ;

bindingVariableDefinition
   : graphVariableDefinition
   | bindingTableVariableDefinition
   | valueVariableDefinition
   ;

statementBlock
   : statement nextStatement*
   ;

statement
   : linearCatalogModifyingStatement
   | linearDataModifyingStatement
   | compositeQueryStatement
   ;

nextStatement
   : NEXT yieldClause? statement
   ;

graphVariableDefinition
   : PROPERTY? GRAPH bindingVariable optTypedGraphInitializer
   ;

optTypedGraphInitializer
   : (typed? graphReferenceValueType)? graphInitializer
   ;

graphInitializer
   : EQUALS_OPERATOR graphExpression
   ;

bindingTableVariableDefinition
   : BINDING? TABLE bindingVariable optTypedBindingTableInitializer
   ;

optTypedBindingTableInitializer
   : (typed? bindingTableReferenceValueType)? bindingTableInitializer
   ;

bindingTableInitializer
   : EQUALS_OPERATOR bindingTableExpression
   ;

valueVariableDefinition
   : VALUE bindingVariable optTypedValueInitializer
   ;

optTypedValueInitializer
   : (typed? valueType)? valueInitializer
   ;

valueInitializer
   : EQUALS_OPERATOR valueExpression
   ;

graphExpression
   : objectExpressionPrimary
   | graphReference
   | objectNameOrBindingVariable
   | currentGraph
   ;

currentGraph
   : CURRENT_PROPERTY_GRAPH
   | CURRENT_GRAPH
   ;

bindingTableExpression
   : nestedBindingTableQuerySpecification
   | objectExpressionPrimary
   | bindingTableReference
   | objectNameOrBindingVariable
   ;

nestedBindingTableQuerySpecification
   : nestedQuerySpecification
   ;

objectExpressionPrimary
   : VARIABLE valueExpressionPrimary
   | parenthesizedValueExpression
   | nonParenthesizedValueExpressionPrimarySpecialCase
   ;

linearCatalogModifyingStatement
   : simpleCatalogModifyingStatement+
   ;

simpleCatalogModifyingStatement
   : primitiveCatalogModifyingStatement
   | callCatalogModifyingProcedureStatement
   ;

primitiveCatalogModifyingStatement
   : createSchemaStatement
   | dropSchemaStatement
   | createGraphStatement
   | dropGraphStatement
   | createGraphTypeStatement
   | dropGraphTypeStatement
   ;

createSchemaStatement
   : CREATE SCHEMA (IF NOT EXISTS)? catalogSchemaParentAndName
   ;

dropSchemaStatement
   : DROP SCHEMA (IF EXISTS)? catalogSchemaParentAndName
   ;

createGraphStatement
   : CREATE (PROPERTY? GRAPH (IF NOT EXISTS)? | OR REPLACE PROPERTY? GRAPH) catalogGraphParentAndName (openGraphType | ofGraphType) graphSource?
   ;

openGraphType
   : typed? ANY (PROPERTY? GRAPH)?
   ;

ofGraphType
   : graphTypeLikeGraph
   | typed? graphTypeReference
   | typed? (PROPERTY? GRAPH)? nestedGraphTypeSpecification
   ;

graphTypeLikeGraph
   : LIKE graphExpression
   ;

graphSource
   : AS COPY OF graphExpression
   ;

dropGraphStatement
   : DROP PROPERTY? GRAPH (IF EXISTS)? catalogGraphParentAndName
   ;

createGraphTypeStatement
   : CREATE (PROPERTY? GRAPH TYPE (IF NOT EXISTS)? | OR REPLACE PROPERTY? GRAPH TYPE) catalogGraphTypeParentAndName graphTypeSource
   ;

graphTypeSource
   : AS? copyOfGraphType
   | graphTypeLikeGraph
   | AS? nestedGraphTypeSpecification
   ;

copyOfGraphType
   : COPY OF (graphTypeReference | externalObjectReference)
   ;

dropGraphTypeStatement
   : DROP PROPERTY? GRAPH TYPE (IF EXISTS)? catalogGraphTypeParentAndName
   ;

callCatalogModifyingProcedureStatement
   : callProcedureStatement
   ;

linearDataModifyingStatement
   : focusedLinearDataModifyingStatement
   | ambientLinearDataModifyingStatement
   ;

focusedLinearDataModifyingStatement
   : focusedLinearDataModifyingStatementBody
   | focusedNestedDataModifyingProcedureSpecification
   ;

focusedLinearDataModifyingStatementBody
   : useGraphClause simpleLinearDataAccessingStatement primitiveResultStatement?
   ;

focusedNestedDataModifyingProcedureSpecification
   : useGraphClause nestedDataModifyingProcedureSpecification
   ;

ambientLinearDataModifyingStatement
   : ambientLinearDataModifyingStatementBody
   | nestedDataModifyingProcedureSpecification
   ;

ambientLinearDataModifyingStatementBody
   : simpleLinearDataAccessingStatement primitiveResultStatement?
   ;

simpleLinearDataAccessingStatement
   : simpleDataAccessingStatement+
   ;

simpleDataAccessingStatement
   : simpleQueryStatement
   | simpleDataModifyingStatement
   ;

simpleDataModifyingStatement
   : primitiveDataModifyingStatement
   | callDataModifyingProcedureStatement
   ;

primitiveDataModifyingStatement
   : insertStatement
   | setStatement
   | removeStatement
   | deleteStatement
   ;

insertStatement
   : INSERT insertGraphPattern
   ;

setStatement
   : SET setItemList
   ;

setItemList
   : setItem (COMMA setItem)*
   ;

setItem
   : setPropertyItem
   | setAllPropertiesItem
   | setLabelItem
   ;

setPropertyItem
   : bindingVariableReference PERIOD propertyName EQUALS_OPERATOR valueExpression
   ;

setAllPropertiesItem
   : bindingVariableReference EQUALS_OPERATOR LEFT_BRACE propertyKeyValuePairList? RIGHT_BRACE
   ;

setLabelItem
   : bindingVariableReference isOrColon labelName
   ;

removeStatement
   : REMOVE removeItemList
   ;

removeItemList
   : removeItem (COMMA removeItem)*
   ;

removeItem
   : removePropertyItem
   | removeLabelItem
   ;

removePropertyItem
   : bindingVariableReference PERIOD propertyName
   ;

removeLabelItem
   : bindingVariableReference isOrColon labelName
   ;

deleteStatement
   : (DETACH | NODETACH)? DELETE deleteItemList
   ;

deleteItemList
   : deleteItem (COMMA deleteItem)*
   ;

deleteItem
   : valueExpression
   ;

callDataModifyingProcedureStatement
   : callProcedureStatement
   ;

compositeQueryStatement
   : compositeQueryExpression
   ;

compositeQueryExpression
   : compositeQueryExpression queryConjunction compositeQueryPrimary
   | compositeQueryPrimary
   ;

queryConjunction
   : setOperator
   | OTHERWISE
   ;

setOperator
   : UNION setQuantifier?
   | EXCEPT setQuantifier?
   | INTERSECT setQuantifier?
   ;

compositeQueryPrimary
   : linearQueryStatement
   ;

linearQueryStatement
   : focusedLinearQueryStatement
   | ambientLinearQueryStatement
   ;

focusedLinearQueryStatement
   : focusedLinearQueryStatementPart* focusedLinearQueryAndPrimitiveResultStatementPart
   | focusedPrimitiveResultStatement
   | focusedNestedQuerySpecification
   | selectStatement
   ;

focusedLinearQueryStatementPart
   : useGraphClause simpleLinearQueryStatement
   ;

focusedLinearQueryAndPrimitiveResultStatementPart
   : useGraphClause simpleLinearQueryStatement primitiveResultStatement
   ;

focusedPrimitiveResultStatement
   : useGraphClause primitiveResultStatement
   ;

focusedNestedQuerySpecification
   : useGraphClause nestedQuerySpecification
   ;

ambientLinearQueryStatement
   : simpleLinearQueryStatement? primitiveResultStatement
   | nestedQuerySpecification
   ;

simpleLinearQueryStatement
   : simpleQueryStatement+
   ;

simpleQueryStatement
   : primitiveQueryStatement
   | callQueryStatement
   ;

primitiveQueryStatement
   : matchStatement
   | letStatement
   | forStatement
   | filterStatement
   | orderByAndPageStatement
   ;

matchStatement
   : simpleMatchStatement
   | optionalMatchStatement
   ;

simpleMatchStatement
   : MATCH graphPatternBindingTable
   ;

optionalMatchStatement
   : OPTIONAL optionalOperand
   ;

optionalOperand
   : simpleMatchStatement
   | LEFT_BRACE matchStatementBlock RIGHT_BRACE
   | LEFT_PAREN matchStatementBlock RIGHT_PAREN
   ;

matchStatementBlock
   : matchStatement+
   ;

callQueryStatement
   : callProcedureStatement
   ;

filterStatement
   : FILTER (whereClause | searchCondition)
   ;

letStatement
   : LET letVariableDefinitionList
   ;

letVariableDefinitionList
   : letVariableDefinition (COMMA letVariableDefinition)*
   ;

letVariableDefinition
   : valueVariableDefinition
   | bindingVariable EQUALS_OPERATOR valueExpression
   ;

forStatement
   : FOR forItem forOrdinalityOrOffset?
   ;

forItem
   : forItemAlias forItemSource
   ;

forItemAlias
   : bindingVariable IN
   ;

forItemSource
   : listValueExpression
   | bindingTableReferenceValueExpression
   ;

forOrdinalityOrOffset
   : WITH (ORDINALITY | OFFSET) bindingVariable
   ;

orderByAndPageStatement
   : orderByClause offsetClause? limitClause?
   | offsetClause limitClause?
   | limitClause
   ;

primitiveResultStatement
   : returnStatement orderByAndPageStatement?
   | FINISH
   ;

returnStatement
   : RETURN returnStatementBody
   ;

returnStatementBody
   : setQuantifier? (ASTERISK | returnItemList) groupByClause?
   | NO BINDINGS
   ;

returnItemList
   : returnItem (COMMA returnItem)*
   ;

returnItem
   : aggregatingValueExpression returnItemAlias?
   ;

returnItemAlias
   : AS IDENTIFIER
   ;

selectStatement
   : SELECT setQuantifier? (ASTERISK | selectItemList) (selectStatementBody whereClause? groupByClause? havingClause? orderByClause? offsetClause? limitClause?)?
   ;

selectItemList
   : selectItem (COMMA selectItem)*
   ;

selectItem
   : aggregatingValueExpression selectItemAlias?
   ;

selectItemAlias
   : AS IDENTIFIER
   ;

havingClause
   : HAVING searchCondition
   ;

selectStatementBody
   : FROM (selectGraphMatchList | selectQuerySpecification)
   ;

selectGraphMatchList
   : selectGraphMatch (COMMA selectGraphMatch)*
   ;

selectGraphMatch
   : graphExpression matchStatement
   ;

selectQuerySpecification
   : nestedQuerySpecification
   | graphExpression nestedQuerySpecification
   ;

callProcedureStatement
   : OPTIONAL? CALL procedureCall
   ;

procedureCall
   : inlineProcedureCall
   | namedProcedureCall
   ;

inlineProcedureCall
   : variableScopeClause? nestedProcedureSpecification
   ;

variableScopeClause
   : LEFT_PAREN bindingVariableReferenceList? RIGHT_PAREN
   ;

bindingVariableReferenceList
   : bindingVariableReference (COMMA bindingVariableReference)*
   ;

namedProcedureCall
   : procedureReference LEFT_PAREN procedureArgumentList? RIGHT_PAREN yieldClause?
   ;

procedureArgumentList
   : procedureArgument (COMMA procedureArgument)*
   ;

procedureArgument
   : valueExpression
   ;

atSchemaClause
   : AT schemaReference
   ;

useGraphClause
   : USE graphExpression
   ;

graphPatternBindingTable
   : graphPattern graphPatternYieldClause?
   ;

graphPatternYieldClause
   : YIELD graphPatternYieldItemList
   ;

graphPatternYieldItemList
   : graphPatternYieldItem (COMMA graphPatternYieldItem)*
   | NO BINDINGS
   ;

graphPatternYieldItem
   : elementVariableReference
   | pathVariableReference
   ;

graphPattern
   : matchMode? pathPatternList keepClause? graphPatternWhereClause?
   ;

matchMode
   : repeatableElementsMatchMode
   | differentEdgesMatchMode
   ;

repeatableElementsMatchMode
   : REPEATABLE elementBindingsOrElements
   ;

differentEdgesMatchMode
   : DIFFERENT edgeBindingsOrEdges
   ;

elementBindingsOrElements
   : ELEMENT BINDINGS?
   | ELEMENTS
   ;

edgeBindingsOrEdges
   : EDGE_SYNONYM BINDINGS?
   | EDGES_SYNONYM
   ;

pathPatternList
   : pathPattern (COMMA pathPattern)*
   ;

pathPattern
   : pathVariableDeclaration? pathPatternPrefix? pathPatternExpression
   ;

pathVariableDeclaration
   : pathVariable EQUALS_OPERATOR
   ;

keepClause
   : KEEP pathPatternPrefix
   ;

graphPatternWhereClause
   : WHERE searchCondition
   ;

insertGraphPattern
   : insertPathPatternList
   ;

insertPathPatternList
   : insertPathPattern (COMMA insertPathPattern)*
   ;

insertPathPattern
   : insertNodePattern (insertEdgePattern insertNodePattern)*
   ;

insertNodePattern
   : LEFT_PAREN insertElementPatternFiller? RIGHT_PAREN
   ;

insertEdgePattern
   : insertEdgePointingLeft
   | insertEdgePointingRight
   | insertEdgeUndirected
   ;

insertEdgePointingLeft
   : LEFT_ARROW_BRACKET insertElementPatternFiller? RIGHT_BRACKET_MINUS
   ;

insertEdgePointingRight
   : MINUS_LEFT_BRACKET insertElementPatternFiller? BRACKET_RIGHT_ARROW
   ;

insertEdgeUndirected
   : TILDE_LEFT_BRACKET insertElementPatternFiller? RIGHT_BRACKET_TILDE
   ;

insertElementPatternFiller
   : elementVariableDeclaration labelAndPropertySetSpecification?
   | elementVariableDeclaration? labelAndPropertySetSpecification
   ;

labelAndPropertySetSpecification
   : isOrColon labelSetSpecification elementPropertySpecification?
   | (isOrColon labelSetSpecification)? elementPropertySpecification
   ;

pathPatternPrefix
   : pathModePrefix
   | pathSearchPrefix
   ;

pathModePrefix
   : pathMode pathOrPaths?
   ;

pathMode
   : WALK
   | TRAIL
   | SIMPLE
   | ACYCLIC
   ;

pathSearchPrefix
   : allPathSearch
   | anyPathSearch
   | shortestPathSearch
   ;

allPathSearch
   : ALL pathMode? pathOrPaths?
   ;

pathOrPaths
   : PATH
   | PATHS
   ;

anyPathSearch
   : ANY numberOfPaths? pathMode? pathOrPaths?
   ;

numberOfPaths
   : nonNegativeIntegerSpecification
   ;

shortestPathSearch
   : allShortestPathSearch
   | anyShortestPathSearch
   | countedShortestPathSearch
   | countedShortestGroupSearch
   ;

allShortestPathSearch
   : ALL SHORTEST pathMode? pathOrPaths?
   ;

anyShortestPathSearch
   : ANY SHORTEST pathMode? pathOrPaths?
   ;

countedShortestPathSearch
   : SHORTEST numberOfPaths pathMode? pathOrPaths?
   ;

countedShortestGroupSearch
   : SHORTEST numberOfGroups? pathMode? pathOrPaths? (GROUP | GROUPS)
   ;

numberOfGroups
   : nonNegativeIntegerSpecification
   ;

pathPatternExpression
   : pathTerm
   | pathMultisetAlternation
   | pathPatternUnion
   ;

pathMultisetAlternation
   : pathTerm MULTISET_ALTERNATION_OPERATOR pathTerm (MULTISET_ALTERNATION_OPERATOR pathTerm)*
   ;

pathPatternUnion
   : pathTerm VERTICAL_BAR pathTerm (VERTICAL_BAR pathTerm)*
   ;

pathTerm
   : pathFactor
   | pathConcatenation
   ;

pathConcatenation
   : pathTerm pathFactor
   ;

pathFactor
   : pathPrimary
   | quantifiedPathPrimary
   | questionedPathPrimary
   ;

quantifiedPathPrimary
   : pathPrimary graphPatternQuantifier
   ;

questionedPathPrimary
   : pathPrimary QUESTION_MARK
   ;

pathPrimary
   : elementPattern
   | parenthesizedPathPatternExpression
   | simplifiedPathPatternExpression
   ;

elementPattern
   : nodePattern
   | edgePattern
   ;

nodePattern
   : LEFT_PAREN elementPatternFiller RIGHT_PAREN
   ;

elementPatternFiller
   : elementVariableDeclaration? isLabelExpression? elementPatternPredicate?
   ;

elementVariableDeclaration
   : TEMP? elementVariable
   ;

isLabelExpression
   : isOrColon labelExpression
   ;

isOrColon
   : IS
   | COLON
   ;

elementPatternPredicate
   : elementPatternWhereClause
   | elementPropertySpecification
   ;

elementPatternWhereClause
   : WHERE searchCondition
   ;

elementPropertySpecification
   : LEFT_BRACE propertyKeyValuePairList RIGHT_BRACE
   ;

propertyKeyValuePairList
   : propertyKeyValuePair (COMMA propertyKeyValuePair)*
   ;

propertyKeyValuePair
   : propertyName COLON valueExpression
   ;

edgePattern
   : fullEdgePattern
   | abbreviatedEdgePattern
   ;

fullEdgePattern
   : fullEdgePointingLeft
   | fullEdgeUndirected
   | fullEdgePointingRight
   | fullEdgeLeftOrUndirected
   | fullEdgeUndirectedOrRight
   | fullEdgeLeftOrRight
   | fullEdgeAnyDirection
   ;

fullEdgePointingLeft
   : LEFT_ARROW_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS
   ;

fullEdgeUndirected
   : TILDE_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE
   ;

fullEdgePointingRight
   : MINUS_LEFT_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW
   ;

fullEdgeLeftOrUndirected
   : LEFT_ARROW_TILDE_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE
   ;

fullEdgeUndirectedOrRight
   : TILDE_LEFT_BRACKET elementPatternFiller BRACKET_TILDE_RIGHT_ARROW
   ;

fullEdgeLeftOrRight
   : LEFT_ARROW_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW
   ;

fullEdgeAnyDirection
   : MINUS_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS
   ;

abbreviatedEdgePattern
   : LEFT_ARROW
   | TILDE
   | RIGHT_ARROW
   | LEFT_ARROW_TILDE
   | TILDE_RIGHT_ARROW
   | LEFT_MINUS_RIGHT
   | MINUS_SIGN
   ;

parenthesizedPathPatternExpression
   : LEFT_PAREN subpathVariableDeclaration? pathModePrefix? pathPatternExpression parenthesizedPathPatternWhereClause? RIGHT_PAREN
   ;

subpathVariableDeclaration
   : subpathVariable EQUALS_OPERATOR
   ;

parenthesizedPathPatternWhereClause
   : WHERE searchCondition
   ;

labelExpression
   : labelTerm
   | labelDisjunction
   ;

labelDisjunction
   : labelExpression VERTICAL_BAR labelTerm
   ;

labelTerm
   : labelFactor
   | labelConjunction
   ;

labelConjunction
   : labelTerm AMPERSAND labelFactor
   ;

labelFactor
   : labelPrimary
   | labelNegation
   ;

labelNegation
   : EXCLAMATION_MARK labelPrimary
   ;

labelPrimary
   : labelName
   | wildcardLabel
   | parenthesizedLabelExpression
   ;

wildcardLabel
   : PERCENT
   ;

parenthesizedLabelExpression
   : LEFT_PAREN labelExpression RIGHT_PAREN
   ;

pathVariableReference
   : bindingVariableReference
   ;

elementVariableReference
   : bindingVariableReference
   ;

graphPatternQuantifier
   : ASTERISK
   | PLUS_SIGN
   | fixedQuantifier
   | generalQuantifier
   ;

fixedQuantifier
   : LEFT_BRACE UNSIGNED_INTEGER RIGHT_BRACE
   ;

generalQuantifier
   : LEFT_BRACE lowerBound? COMMA upperBound? RIGHT_BRACE
   ;

lowerBound
   : UNSIGNED_INTEGER
   ;

upperBound
   : UNSIGNED_INTEGER
   ;

simplifiedPathPatternExpression
   : simplifiedDefaultingLeft
   | simplifiedDefaultingUndirected
   | simplifiedDefaultingRight
   | simplifiedDefaultingLeftOrUndirected
   | simplifiedDefaultingUndirectedOrRight
   | simplifiedDefaultingLeftOrRight
   | simplifiedDefaultingAnyDirection
   ;

simplifiedDefaultingLeft
   : LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS
   ;

simplifiedDefaultingUndirected
   : TILDE_SLASH simplifiedContents SLASH_TILDE
   ;

simplifiedDefaultingRight
   : MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT
   ;

simplifiedDefaultingLeftOrUndirected
   : LEFT_TILDE_SLASH simplifiedContents SLASH_TILDE
   ;

simplifiedDefaultingUndirectedOrRight
   : TILDE_SLASH simplifiedContents SLASH_TILDE_RIGHT
   ;

simplifiedDefaultingLeftOrRight
   : LEFT_MINUS_SLASH simplifiedContents SLASH_MINUS_RIGHT
   ;

simplifiedDefaultingAnyDirection
   : MINUS_SLASH simplifiedContents SLASH_MINUS
   ;

simplifiedContents
   : simplifiedTerm
   | simplifiedPathUnion
   | simplifiedMultisetAlternation
   ;

simplifiedPathUnion
   : simplifiedTerm VERTICAL_BAR simplifiedTerm (VERTICAL_BAR simplifiedTerm)*
   ;

simplifiedMultisetAlternation
   : simplifiedTerm MULTISET_ALTERNATION_OPERATOR simplifiedTerm (MULTISET_ALTERNATION_OPERATOR simplifiedTerm)*
   ;

simplifiedTerm
   : simplifiedFactorLow
   | simplifiedConcatenation
   ;

simplifiedConcatenation
   : simplifiedTerm simplifiedFactorLow
   ;

simplifiedFactorLow
   : simplifiedFactorHigh
   | simplifiedConjunction
   ;

simplifiedConjunction
   : simplifiedFactorLow AMPERSAND simplifiedFactorHigh
   ;

simplifiedFactorHigh
   : simplifiedTertiary
   | simplifiedQuantified
   | simplifiedQuestioned
   ;

simplifiedQuantified
   : simplifiedTertiary graphPatternQuantifier
   ;

simplifiedQuestioned
   : simplifiedTertiary QUESTION_MARK
   ;

simplifiedTertiary
   : simplifiedDirectionOverride
   | simplifiedSecondary
   ;

simplifiedDirectionOverride
   : simplifiedOverrideLeft
   | simplifiedOverrideUndirected
   | simplifiedOverrideRight
   | simplifiedOverrideLeftOrUndirected
   | simplifiedOverrideUndirectedOrRight
   | simplifiedOverrideLeftOrRight
   | simplifiedOverrideAnyDirection
   ;

simplifiedOverrideLeft
   : LEFT_ANGLE_BRACKET simplifiedSecondary
   ;

simplifiedOverrideUndirected
   : TILDE simplifiedSecondary
   ;

simplifiedOverrideRight
   : simplifiedSecondary RIGHT_ANGLE_BRACKET
   ;

simplifiedOverrideLeftOrUndirected
   : LEFT_ARROW_TILDE simplifiedSecondary
   ;

simplifiedOverrideUndirectedOrRight
   : TILDE simplifiedSecondary RIGHT_ANGLE_BRACKET
   ;

simplifiedOverrideLeftOrRight
   : LEFT_ANGLE_BRACKET simplifiedSecondary RIGHT_ANGLE_BRACKET
   ;

simplifiedOverrideAnyDirection
   : MINUS_SIGN simplifiedSecondary
   ;

simplifiedSecondary
   : simplifiedPrimary
   | simplifiedNegation
   ;

simplifiedNegation
   : EXCLAMATION_MARK simplifiedPrimary
   ;

simplifiedPrimary
   : labelName
   | LEFT_PAREN simplifiedContents RIGHT_PAREN
   ;

whereClause
   : WHERE searchCondition
   ;

yieldClause
   : YIELD yieldItemList
   ;

yieldItemList
   : yieldItem (COMMA yieldItem)*
   ;

yieldItem
   : (yieldItemName yieldItemAlias?)
   ;

yieldItemName
   : fieldName
   ;

yieldItemAlias
   : AS bindingVariable
   ;

groupByClause
   : GROUP BY groupingElementList
   ;

groupingElementList
   : groupingElement (COMMA groupingElement)*
   | emptyGroupingSet
   ;

groupingElement
   : bindingVariableReference
   ;

emptyGroupingSet
   : LEFT_PAREN RIGHT_PAREN
   ;

orderByClause
   : ORDER BY sortSpecificationList
   ;

sortSpecificationList
   : sortSpecification (COMMA sortSpecification)*
   ;

sortSpecification
   : sortKey orderingSpecification? nullOrdering?
   ;

sortKey
   : aggregatingValueExpression
   ;

orderingSpecification
   : ASC
   | ASCENDING
   | DESC
   | DESCENDING
   ;

nullOrdering
   : NULLS FIRST
   | NULLS LAST
   ;

limitClause
   : LIMIT nonNegativeIntegerSpecification
   ;

offsetClause
   : offsetSynonym nonNegativeIntegerSpecification
   ;

offsetSynonym
   : OFFSET
   | SKIP_
   ;

schemaReference
   : absoluteCatalogSchemaReference
   | relativeCatalogSchemaReference
   | referenceParameterSpecification
   ;

absoluteCatalogSchemaReference
   : SOLIDUS
   | absoluteDirectoryPath schemaName
   ;

catalogSchemaParentAndName
   : absoluteDirectoryPath schemaName
   ;

relativeCatalogSchemaReference
   : predefinedSchemaReference
   | relativeDirectoryPath schemaName
   ;

predefinedSchemaReference
   : HOME_SCHEMA
   | CURRENT_SCHEMA
   | PERIOD
   ;

absoluteDirectoryPath
   : SOLIDUS simpleDirectoryPath?
   ;

relativeDirectoryPath
   : DOUBLE_PERIOD ( (SOLIDUS DOUBLE_PERIOD)+ SOLIDUS simpleDirectoryPath?)?
   ;

simpleDirectoryPath
   : (directoryName SOLIDUS)+
   ;

graphReference
   : catalogObjectParentReference graphName
   | delimitedGraphName
   | homeGraph
   | referenceParameterSpecification
   ;

catalogGraphParentAndName
   : catalogObjectParentReference? graphName
   ;

homeGraph
   : HOME_PROPERTY_GRAPH
   | HOME_GRAPH
   ;

graphTypeReference
   : catalogGraphTypeParentAndName
   | referenceParameterSpecification
   ;

catalogGraphTypeParentAndName
   : catalogObjectParentReference? graphTypeName
   ;

bindingTableReference
   : catalogObjectParentReference bindingTableName
   | delimitedBindingTableName
   | referenceParameterSpecification
   ;

catalogBindingTableParentAndName
   : catalogObjectParentReference? bindingTableName
   ;

procedureReference
   : catalogProcedureParentAndName
   | referenceParameterSpecification
   ;

catalogProcedureParentAndName
   : catalogObjectParentReference? procedureName
   ;

catalogObjectParentReference
   : schemaReference SOLIDUS? (objectName PERIOD)*
   |  (objectName PERIOD)+
   ;

referenceParameterSpecification
   : SUBSTITUTED_PARAMETER_REFERENCE
   ;

externalObjectReference
   : 
   ;

nestedGraphTypeSpecification
   : LEFT_BRACE graphTypeSpecificationBody RIGHT_BRACE
   ;

graphTypeSpecificationBody
   : elementTypeList
   ;

elementTypeList
   : elementTypeSpecification (COMMA elementTypeSpecification)*
   ;

elementTypeSpecification
   : nodeTypeSpecification
   | edgeTypeSpecification
   ;

nodeTypeSpecification
   : nodeTypePattern
   | nodeTypePhrase
   ;

nodeTypePattern
   : (NODE_SYNONYM TYPE? nodeTypeName)? LEFT_PAREN localNodeTypeAlias? nodeTypeFiller? RIGHT_PAREN
   ;

nodeTypePhrase
   : NODE_SYNONYM TYPE? nodeTypePhraseFiller (AS localNodeTypeAlias)?
   ;

nodeTypePhraseFiller
   : nodeTypeName nodeTypeFiller?
   | nodeTypeFiller
   ;

nodeTypeFiller
   : nodeTypeKeyLabelSet nodeTypeImpliedContent?
   | nodeTypeImpliedContent
   ;

localNodeTypeAlias
   : REGULAR_IDENTIFIER
   ;

nodeTypeImpliedContent
   : nodeTypeLabelSet
   | nodeTypePropertyTypes
   | nodeTypeLabelSet nodeTypePropertyTypes
   ;

nodeTypeKeyLabelSet
   : labelSetPhrase? IMPLIES
   ;

nodeTypeLabelSet
   : labelSetPhrase
   ;

nodeTypePropertyTypes
   : propertyTypesSpecification
   ;

edgeTypeSpecification
   : edgeTypePattern
   | edgeTypePhrase
   ;

edgeTypePattern
   : (edgeKind? EDGE_SYNONYM TYPE? edgeTypeName)? (edgeTypePatternDirected | edgeTypePatternUndirected)
   ;

edgeTypePhrase
   : edgeKind EDGE_SYNONYM TYPE? edgeTypePhraseFiller endpointPairPhrase
   ;

edgeTypePhraseFiller
   : edgeTypeName edgeTypeFiller?
   | edgeTypeFiller
   ;

edgeTypeFiller
   : edgeTypeKeyLabelSet edgeTypeImpliedContent?
   | edgeTypeImpliedContent
   ;

edgeTypeImpliedContent
   : edgeTypeLabelSet
   | edgeTypePropertyTypes
   | edgeTypeLabelSet edgeTypePropertyTypes
   ;

edgeTypeKeyLabelSet
   : labelSetPhrase? IMPLIES
   ;

edgeTypeLabelSet
   : labelSetPhrase
   ;

edgeTypePropertyTypes
   : propertyTypesSpecification
   ;

edgeTypePatternDirected
   : edgeTypePatternPointingRight
   | edgeTypePatternPointingLeft
   ;

edgeTypePatternPointingRight
   : sourceNodeTypeReference arcTypePointingRight destinationNodeTypeReference
   ;

edgeTypePatternPointingLeft
   : destinationNodeTypeReference arcTypePointingLeft sourceNodeTypeReference
   ;

edgeTypePatternUndirected
   : sourceNodeTypeReference arcTypeUndirected destinationNodeTypeReference
   ;

arcTypePointingRight
   : MINUS_LEFT_BRACKET edgeTypeFiller BRACKET_RIGHT_ARROW
   ;

arcTypePointingLeft
   : LEFT_ARROW_BRACKET edgeTypeFiller RIGHT_BRACKET_MINUS
   ;

arcTypeUndirected
   : TILDE_LEFT_BRACKET edgeTypeFiller RIGHT_BRACKET_TILDE
   ;

sourceNodeTypeReference
   : LEFT_PAREN sourceNodeTypeAlias RIGHT_PAREN
   | LEFT_PAREN nodeTypeFiller? RIGHT_PAREN
   ;

destinationNodeTypeReference
   : LEFT_PAREN destinationNodeTypeAlias RIGHT_PAREN
   | LEFT_PAREN nodeTypeFiller? RIGHT_PAREN
   ;

edgeKind
   : DIRECTED
   | UNDIRECTED
   ;

endpointPairPhrase
   : CONNECTING endpointPair
   ;

endpointPair
   : endpointPairDirected
   | endpointPairUndirected
   ;

endpointPairDirected
   : endpointPairPointingRight
   | endpointPairPointingLeft
   ;

endpointPairPointingRight
   : LEFT_PAREN sourceNodeTypeAlias connectorPointingRight destinationNodeTypeAlias RIGHT_PAREN
   ;

endpointPairPointingLeft
   : LEFT_PAREN destinationNodeTypeAlias LEFT_ARROW sourceNodeTypeAlias RIGHT_PAREN
   ;

endpointPairUndirected
   : LEFT_PAREN sourceNodeTypeAlias connectorUndirected destinationNodeTypeAlias RIGHT_PAREN
   ;

connectorPointingRight
   : TO
   | RIGHT_ARROW
   ;

connectorUndirected
   : TO
   | TILDE
   ;

sourceNodeTypeAlias
   : REGULAR_IDENTIFIER
   ;

destinationNodeTypeAlias
   : REGULAR_IDENTIFIER
   ;

labelSetPhrase
   : LABEL labelName
   | LABELS labelSetSpecification
   | isOrColon labelSetSpecification
   ;

labelSetSpecification
   : labelName (AMPERSAND labelName)*
   ;

propertyTypesSpecification
   : LEFT_BRACE propertyTypeList? RIGHT_BRACE
   ;

propertyTypeList
   : propertyType (COMMA propertyType)*
   ;

propertyType
   : propertyName typed? propertyValueType
   ;

propertyValueType
   : valueType
   ;

bindingTableType
   : BINDING? TABLE fieldTypesSpecification
   ;

valueType
   : predefinedType
   | constructedValueType
   | dynamicUnionType
   ;

typed
   : DOUBLE_COLON
   | TYPED
   ;

predefinedType
   : booleanType
   | characterStringType
   | byteStringType
   | numericType
   | temporalType
   | referenceValueType
   | immaterialValueType
   ;

booleanType
   : (BOOL | BOOLEAN) notNull?
   ;

characterStringType
   : STRING (LEFT_PAREN (minLength COMMA)? maxLength RIGHT_PAREN)? notNull?
   | CHAR (LEFT_PAREN fixedLength RIGHT_PAREN)? notNull?
   | VARCHAR (LEFT_PAREN maxLength RIGHT_PAREN)? notNull?
   ;

byteStringType
   : BYTES (LEFT_PAREN (minLength COMMA)? maxLength RIGHT_PAREN)? notNull?
   | BINARY (LEFT_PAREN fixedLength RIGHT_PAREN)? notNull?
   | VARBINARY (LEFT_PAREN maxLength RIGHT_PAREN)? notNull?
   ;

minLength
   : UNSIGNED_INTEGER
   ;

maxLength
   : UNSIGNED_INTEGER
   ;

fixedLength
   : UNSIGNED_INTEGER
   ;

numericType
   : exactNumericType
   | approximateNumericType
   ;

exactNumericType
   : binaryExactNumericType
   | decimalExactNumericType
   ;

binaryExactNumericType
   : signedBinaryExactNumericType
   | unsignedBinaryExactNumericType
   ;

signedBinaryExactNumericType
   : INT8 notNull?
   | INT16 notNull?
   | INT32 notNull?
   | INT64 notNull?
   | INT128 notNull?
   | INT256 notNull?
   | SMALLINT notNull?
   | INT (LEFT_PAREN precision RIGHT_PAREN)? notNull?
   | BIGINT notNull?
   | SIGNED? verboseBinaryExactNumericType
   ;

unsignedBinaryExactNumericType
   : UINT8 notNull?
   | UINT16 notNull?
   | UINT32 notNull?
   | UINT64 notNull?
   | UINT128 notNull?
   | UINT256 notNull?
   | USMALLINT notNull?
   | UINT (LEFT_PAREN precision RIGHT_PAREN)? notNull?
   | UBIGINT notNull?
   | UNSIGNED verboseBinaryExactNumericType
   ;

verboseBinaryExactNumericType
   : INTEGER8 notNull?
   | INTEGER16 notNull?
   | INTEGER32 notNull?
   | INTEGER64 notNull?
   | INTEGER128 notNull?
   | INTEGER256 notNull?
   | SMALL INTEGER notNull?
   | INTEGER (LEFT_PAREN precision RIGHT_PAREN)? notNull?
   | BIG INTEGER notNull?
   ;

decimalExactNumericType
   : (DECIMAL | DEC) (LEFT_PAREN precision (COMMA scale)? RIGHT_PAREN notNull?)?
   ;

precision
   : UNSIGNED_DECIMAL_INTEGER
   ;

scale
   : UNSIGNED_DECIMAL_INTEGER
   ;

approximateNumericType
   : FLOAT16 notNull?
   | FLOAT32 notNull?
   | FLOAT64 notNull?
   | FLOAT128 notNull?
   | FLOAT256 notNull?
   | FLOAT (LEFT_PAREN precision (COMMA scale)? RIGHT_PAREN)? notNull?
   | REAL notNull?
   | DOUBLE PRECISION? notNull?
   ;

temporalType
   : temporalInstantType
   | temporalDurationType
   ;

temporalInstantType
   : datetimeType
   | localdatetimeType
   | dateType
   | timeType
   | localtimeType
   ;

datetimeType
   : ZONED DATETIME notNull?
   | TIMESTAMP WITH TIME ZONE notNull?
   ;

localdatetimeType
   : LOCAL DATETIME notNull?
   | TIMESTAMP (WITHOUT TIME ZONE)? notNull?
   ;

dateType
   : DATE notNull?
   ;

timeType
   : ZONED TIME notNull?
   | TIME WITH TIME ZONE notNull?
   ;

localtimeType
   : LOCAL TIME notNull?
   | TIME WITHOUT TIME ZONE notNull?
   ;

temporalDurationType
   : DURATION LEFT_PAREN temporalDurationQualifier RIGHT_PAREN notNull?
   ;

temporalDurationQualifier
   : YEAR TO MONTH
   | DAY TO SECOND
   ;

referenceValueType
   : graphReferenceValueType
   | bindingTableReferenceValueType
   | nodeReferenceValueType
   | edgeReferenceValueType
   ;

immaterialValueType
   : nullType
   | emptyType
   ;

nullType
   : NULL
   ;

emptyType
   : NULL notNull
   | NOTHING
   ;

graphReferenceValueType
   : openGraphReferenceValueType
   | closedGraphReferenceValueType
   ;

closedGraphReferenceValueType
   : PROPERTY? GRAPH nestedGraphTypeSpecification notNull?
   ;

openGraphReferenceValueType
   : ANY PROPERTY? GRAPH notNull?
   ;

bindingTableReferenceValueType
   : bindingTableType notNull?
   ;

nodeReferenceValueType
   : openNodeReferenceValueType
   | closedNodeReferenceValueType
   ;

closedNodeReferenceValueType
   : nodeTypeSpecification notNull?
   ;

openNodeReferenceValueType
   : ANY? NODE_SYNONYM notNull?
   ;

edgeReferenceValueType
   : openEdgeReferenceValueType
   | closedEdgeReferenceValueType
   ;

closedEdgeReferenceValueType
   : edgeTypeSpecification notNull?
   ;

openEdgeReferenceValueType
   : ANY? EDGE_SYNONYM notNull?
   ;

constructedValueType
   : pathValueType
   | listValueType
   | recordType
   ;

pathValueType
   : PATH notNull?
   ;

listValueType
   : (listValueTypeName LEFT_ANGLE_BRACKET valueType RIGHT_ANGLE_BRACKET | valueType? listValueTypeName) (LEFT_BRACKET maxLength RIGHT_BRACKET)? notNull?
   ;

listValueTypeName
   : GROUP? listValueTypeNameSynonym
   ;

listValueTypeNameSynonym
   : LIST
   | ARRAY
   ;

recordType
   : ANY? RECORD notNull?
   | RECORD? fieldTypesSpecification notNull?
   ;

fieldTypesSpecification
   : LEFT_BRACE fieldTypeList? RIGHT_BRACE
   ;

fieldTypeList
   : fieldType (COMMA fieldType)*
   ;

dynamicUnionType
   : openDynamicUnionType
   | dynamicPropertyValueType
   | closedDynamicUnionType
   ;

openDynamicUnionType
   : ANY VALUE? notNull?
   ;

dynamicPropertyValueType
   : ANY? PROPERTY VALUE notNull?
   ;

closedDynamicUnionType
   : ANY VALUE? LEFT_ANGLE_BRACKET componentTypeList RIGHT_ANGLE_BRACKET
   | componentTypeList
   ;

componentTypeList
   : componentType (VERTICAL_BAR componentType)*
   ;

componentType
   : valueType
   ;

notNull
   :  NOT NULL
   ;

fieldType
   : fieldName typed? valueType
   ;

searchCondition
   : booleanValueExpression
   ;

predicate
   : comparisonPredicate
   | existsPredicate
   | nullPredicate
   | normalizedPredicate
   | valueTypePredicate
   | directedPredicate
   | labeledPredicate
   | sourceDestinationPredicate
   | all_differentPredicate
   | samePredicate
   | property_existsPredicate
   ;

comparisonPredicate
   : comparisonPredicand comparisonPredicatePart2
   ;

comparisonPredicatePart2
   : compOp comparisonPredicand
   ;

compOp
   : EQUALS_OPERATOR
   | NOT_EQUALS_OPERATOR
   | LESS_THAN_OPERATOR
   | GREATER_THAN_OPERATOR
   | LESS_THAN_OR_EQUALS_OPERATOR
   | GREATER_THAN_OR_EQUALS_OPERATOR
   ;

comparisonPredicand
   : commonValueExpression
   | booleanPredicand
   ;

existsPredicate
   : EXISTS (LEFT_BRACE graphPattern RIGHT_BRACE | LEFT_PAREN graphPattern RIGHT_PAREN | LEFT_BRACE matchStatementBlock RIGHT_BRACE | LEFT_PAREN matchStatementBlock RIGHT_PAREN | nestedQuerySpecification)
   ;

nullPredicate
   : valueExpressionPrimary nullPredicatePart2
   ;

nullPredicatePart2
   : IS NOT? NULL
   ;

valueTypePredicate
   : valueExpressionPrimary valueTypePredicatePart2
   ;

valueTypePredicatePart2
   : IS NOT? typed valueType
   ;

normalizedPredicate
   : stringValueExpression normalizedPredicatePart2
   ;

normalizedPredicatePart2
   : IS NOT? normalForm? NORMALIZED
   ;

directedPredicate
   : elementVariableReference directedPredicatePart2
   ;

directedPredicatePart2
   : IS NOT? DIRECTED
   ;

labeledPredicate
   : elementVariableReference labeledPredicatePart2
   ;

labeledPredicatePart2
   : isLabeledOrColon labelExpression
   ;

isLabeledOrColon
   : IS NOT? LABELED
   | COLON
   ;

sourceDestinationPredicate
   : nodeReference sourcePredicatePart2
   | nodeReference destinationPredicatePart2
   ;

nodeReference
   : elementVariableReference
   ;

sourcePredicatePart2
   : IS NOT? SOURCE OF edgeReference
   ;

destinationPredicatePart2
   : IS NOT? DESTINATION OF edgeReference
   ;

edgeReference
   : elementVariableReference
   ;

all_differentPredicate
   : ALL_DIFFERENT LEFT_PAREN elementVariableReference COMMA elementVariableReference (COMMA elementVariableReference)* RIGHT_PAREN
   ;

samePredicate
   : SAME LEFT_PAREN elementVariableReference COMMA elementVariableReference (COMMA elementVariableReference)* RIGHT_PAREN
   ;

property_existsPredicate
   : PROPERTY_EXISTS LEFT_PAREN elementVariableReference COMMA propertyName RIGHT_PAREN
   ;

valueExpression
   : commonValueExpression
   | booleanValueExpression
   ;

commonValueExpression
   : numericValueExpression
   | stringValueExpression
   | datetimeValueExpression
   | durationValueExpression
   | listValueExpression
   | recordExpression
   | pathValueExpression
   | referenceValueExpression
   ;

referenceValueExpression
   : graphReferenceValueExpression
   | bindingTableReferenceValueExpression
   | nodeReferenceValueExpression
   | edgeReferenceValueExpression
   ;

graphReferenceValueExpression
   : PROPERTY? GRAPH graphExpression
   | valueExpressionPrimary
   ;

bindingTableReferenceValueExpression
   : BINDING? TABLE bindingTableExpression
   | valueExpressionPrimary
   ;

nodeReferenceValueExpression
   : valueExpressionPrimary
   ;

edgeReferenceValueExpression
   : valueExpressionPrimary
   ;

recordExpression
   : valueExpressionPrimary
   ;

aggregatingValueExpression
   : valueExpression
   ;

valueExpressionPrimary
   : parenthesizedValueExpression
   | nonParenthesizedValueExpressionPrimary
   ;

parenthesizedValueExpression
   : LEFT_PAREN valueExpression RIGHT_PAREN
   ;

nonParenthesizedValueExpressionPrimary
   : nonParenthesizedValueExpressionPrimarySpecialCase
   | bindingVariableReference
   ;

nonParenthesizedValueExpressionPrimarySpecialCase
   : aggregateFunction
   | unsignedValueSpecification
   | listValueConstructor
   | recordConstructor
   | pathValueConstructor
   | propertyReference
   | valueQueryExpression
   | caseExpression
   | castSpecification
   | element_idFunction
   | letValueExpression
   ;

valueSpecification
   : literal
   | generalValueSpecification
   ;

unsignedValueSpecification
   : unsignedLiteral
   | generalValueSpecification
   ;

nonNegativeIntegerSpecification
   : UNSIGNED_INTEGER
   | dynamicParameterSpecification
   ;

generalValueSpecification
   : dynamicParameterSpecification
   | SESSION_USER
   ;

dynamicParameterSpecification
   : GENERAL_PARAMETER_REFERENCE
   ;

letValueExpression
   : LET letVariableDefinitionList IN valueExpression END
   ;

valueQueryExpression
   : VALUE nestedQuerySpecification
   ;

caseExpression
   : caseAbbreviation
   | caseSpecification
   ;

caseAbbreviation
   : NULLIF LEFT_PAREN valueExpression COMMA valueExpression RIGHT_PAREN
   | COALESCE LEFT_PAREN valueExpression (COMMA valueExpression)+ RIGHT_PAREN
   ;

caseSpecification
   : simpleCase
   | searchedCase
   ;

simpleCase
   : CASE caseOperand simpleWhenClause+ elseClause? END
   ;

searchedCase
   : CASE searchedWhenClause+ elseClause? END
   ;

simpleWhenClause
   : WHEN whenOperandList THEN result
   ;

searchedWhenClause
   : WHEN searchCondition THEN result
   ;

elseClause
   : ELSE result
   ;

caseOperand
   : nonParenthesizedValueExpressionPrimary
   | elementVariableReference
   ;

whenOperandList
   : whenOperand (COMMA whenOperand)*
   ;

whenOperand
   : nonParenthesizedValueExpressionPrimary
   | comparisonPredicatePart2
   | nullPredicatePart2
   | valueTypePredicatePart2
   | normalizedPredicatePart2
   | directedPredicatePart2
   | labeledPredicatePart2
   | sourcePredicatePart2
   | destinationPredicatePart2
   ;

result
   : resultExpression
   | NULL_LITERAL
   ;

resultExpression
   : valueExpression
   ;

castSpecification
   : CAST LEFT_PAREN castOperand AS castTarget RIGHT_PAREN
   ;

castOperand
   : valueExpression
   | NULL_LITERAL
   ;

castTarget
   : valueType
   ;

aggregateFunction
   : COUNT LEFT_PAREN ASTERISK RIGHT_PAREN
   | generalSetFunction
   | binarySetFunction
   ;

generalSetFunction
   : generalSetFunctionType LEFT_PAREN setQuantifier? valueExpression RIGHT_PAREN
   ;

binarySetFunction
   : binarySetFunctionType LEFT_PAREN dependentValueExpression COMMA independentValueExpression RIGHT_PAREN
   ;

generalSetFunctionType
   : AVG
   | COUNT
   | MAX
   | MIN
   | SUM
   | COLLECT_LIST
   | STDDEV_SAMP
   | STDDEV_POP
   ;

setQuantifier
   : DISTINCT
   | ALL
   ;

binarySetFunctionType
   : PERCENTILE_CONT
   | PERCENTILE_DISC
   ;

dependentValueExpression
   : setQuantifier? numericValueExpression
   ;

independentValueExpression
   : numericValueExpression
   ;

element_idFunction
   : ELEMENT_ID LEFT_PAREN elementVariableReference RIGHT_PAREN
   ;

propertyReference
   : propertySource PERIOD propertyName
   ;

propertySource
   : nodeReferenceValueExpression
   | edgeReferenceValueExpression
   | recordExpression
   ;

bindingVariableReference
   : bindingVariable
   ;

pathValueExpression
   : pathValueConcatenation
   | pathValuePrimary
   ;

pathValueConcatenation
   : pathValueExpression1 CONCATENATION_OPERATOR pathValuePrimary
   ;

pathValueExpression1
   : pathValueExpression
   ;

pathValuePrimary
   : valueExpressionPrimary
   ;

pathValueConstructor
   : pathValueConstructorByEnumeration
   ;

pathValueConstructorByEnumeration
   : PATH LEFT_BRACKET pathElementList RIGHT_BRACKET
   ;

pathElementList
   : pathElementListStart pathElementListStep*
   ;

pathElementListStart
   : nodeReferenceValueExpression
   ;

pathElementListStep
   : COMMA edgeReferenceValueExpression COMMA nodeReferenceValueExpression
   ;

listValueExpression
   : listConcatenation
   | listPrimary
   ;

listConcatenation
   : listValueExpression1 CONCATENATION_OPERATOR listPrimary
   ;

listValueExpression1
   : listValueExpression
   ;

listPrimary
   : listValueFunction
   | valueExpressionPrimary
   ;

listValueFunction
   : trimListFunction
   | elementsFunction
   ;

trimListFunction
   : TRIM LEFT_PAREN listValueExpression COMMA numericValueExpression RIGHT_PAREN
   ;

elementsFunction
   : ELEMENTS LEFT_PAREN pathValueExpression RIGHT_PAREN
   ;

listValueConstructor
   : listValueConstructorByEnumeration
   ;

listValueConstructorByEnumeration
   : listValueTypeName? LEFT_BRACKET listElementList? RIGHT_BRACKET
   ;

listElementList
   : listElement (COMMA listElement)*
   ;

listElement
   : valueExpression
   ;

recordConstructor
   : RECORD? fieldsSpecification
   ;

fieldsSpecification
   : LEFT_BRACE fieldList? RIGHT_BRACE
   ;

fieldList
   : field (COMMA field)*
   ;

field
   : fieldName COLON valueExpression
   ;

booleanValueExpression
   : booleanTerm
   | booleanValueExpression OR booleanTerm
   | booleanValueExpression XOR booleanTerm
   ;

booleanTerm
   : booleanFactor
   | booleanTerm AND booleanFactor
   ;

booleanFactor
   : NOT? booleanTest
   ;

booleanTest
   : booleanPrimary (IS NOT? truthValue)?
   ;

truthValue
   : TRUE
   | FALSE
   | UNKNOWN
   ;

booleanPrimary
   : predicate
   | booleanPredicand
   ;

booleanPredicand
   : parenthesizedBooleanValueExpression
   | nonParenthesizedValueExpressionPrimary
   ;

parenthesizedBooleanValueExpression
   : LEFT_PAREN booleanValueExpression RIGHT_PAREN
   ;

numericValueExpression
   : term
   | numericValueExpression PLUS_SIGN term
   | numericValueExpression MINUS_SIGN term
   ;

term
   : factor
   | term ASTERISK factor
   | term SOLIDUS factor
   ;

factor
   : SIGN? numericPrimary
   ;

numericPrimary
   : valueExpressionPrimary
   | numericValueFunction
   ;

numericValueFunction
   : lengthExpression
   | cardinalityExpression
   | absoluteValueExpression
   | modulusExpression
   | trigonometricFunction
   | generalLogarithmFunction
   | commonLogarithm
   | naturalLogarithm
   | exponentialFunction
   | powerFunction
   | squareRoot
   | floorFunction
   | ceilingFunction
   ;

lengthExpression
   : charLengthExpression
   | byteLengthExpression
   | pathLengthExpression
   ;

cardinalityExpression
   : CARDINALITY LEFT_PAREN cardinalityExpressionArgument RIGHT_PAREN
   | SIZE LEFT_PAREN listValueExpression RIGHT_PAREN
   ;

cardinalityExpressionArgument
   : bindingTableReferenceValueExpression
   | pathValueExpression
   | listValueExpression
   | recordExpression
   ;

charLengthExpression
   : (CHAR_LENGTH | CHARACTER_LENGTH) LEFT_PAREN characterStringValueExpression RIGHT_PAREN
   ;

byteLengthExpression
   : (BYTE_LENGTH | OCTET_LENGTH) LEFT_PAREN byteStringValueExpression RIGHT_PAREN
   ;

pathLengthExpression
   : PATH_LENGTH LEFT_PAREN pathValueExpression RIGHT_PAREN
   ;

absoluteValueExpression
   : ABS LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

modulusExpression
   : MOD LEFT_PAREN numericValueExpressionDividend COMMA numericValueExpressionDivisor RIGHT_PAREN
   ;

numericValueExpressionDividend
   : numericValueExpression
   ;

numericValueExpressionDivisor
   : numericValueExpression
   ;

trigonometricFunction
   : trigonometricFunctionName LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

trigonometricFunctionName
   : SIN
   | COS
   | TAN
   | COT
   | SINH
   | COSH
   | TANH
   | ASIN
   | ACOS
   | ATAN
   | DEGREES
   | RADIANS
   ;

generalLogarithmFunction
   : LOG LEFT_PAREN generalLogarithmBase COMMA generalLogarithmArgument RIGHT_PAREN
   ;

generalLogarithmBase
   : numericValueExpression
   ;

generalLogarithmArgument
   : numericValueExpression
   ;

commonLogarithm
   : LOG10 LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

naturalLogarithm
   : LN LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

exponentialFunction
   : EXP LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

powerFunction
   : POWER LEFT_PAREN numericValueExpressionBase COMMA numericValueExpressionExponent RIGHT_PAREN
   ;

numericValueExpressionBase
   : numericValueExpression
   ;

numericValueExpressionExponent
   : numericValueExpression
   ;

squareRoot
   : SQRT LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

floorFunction
   : FLOOR LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

ceilingFunction
   : (CEIL | CEILING) LEFT_PAREN numericValueExpression RIGHT_PAREN
   ;

stringValueExpression
   : characterStringValueExpression
   | byteStringValueExpression
   ;

characterStringValueExpression
   : characterStringConcatenation
   | characterStringPrimary
   ;

characterStringConcatenation
   : characterStringValueExpression CONCATENATION_OPERATOR characterStringPrimary
   ;

characterStringPrimary
   : valueExpressionPrimary
   | characterStringFunction
   ;

byteStringValueExpression
   : byteStringConcatenation
   | byteStringPrimary
   ;

byteStringPrimary
   : valueExpressionPrimary
   | byteStringFunction
   ;

byteStringConcatenation
   : byteStringValueExpression CONCATENATION_OPERATOR byteStringPrimary
   ;

characterStringFunction
   : substringFunction
   | fold
   | trimFunction
   | normalizeFunction
   ;

substringFunction
   : (LEFT | RIGHT) LEFT_PAREN characterStringValueExpression COMMA stringLength RIGHT_PAREN
   ;

fold
   : (UPPER | LOWER) LEFT_PAREN characterStringValueExpression RIGHT_PAREN
   ;

trimFunction
   : singleCharacterTrimFunction
   | multiCharacterTrimFunction
   ;

singleCharacterTrimFunction
   : TRIM LEFT_PAREN trimOperands RIGHT_PAREN
   ;

multiCharacterTrimFunction
   : (BTRIM | LTRIM | RTRIM) LEFT_PAREN trimSource (COMMA trimCharacterString)? RIGHT_PAREN
   ;

trimOperands
   : (trimSpecification? trimCharacterString? FROM)? trimSource
   ;

trimSource
   : characterStringValueExpression
   ;

trimSpecification
   : LEADING
   | TRAILING
   | BOTH
   ;

trimCharacterString
   : characterStringValueExpression
   ;

normalizeFunction
   : NORMALIZE LEFT_PAREN characterStringValueExpression (COMMA normalForm)? RIGHT_PAREN
   ;

normalForm
   : NFC
   | NFD
   | NFKC
   | NFKD
   ;

stringLength
   : numericValueExpression
   ;

byteStringFunction
   : byteStringSubstringFunction
   | byteStringTrimFunction
   ;

byteStringSubstringFunction
   : (LEFT | RIGHT) LEFT_PAREN byteStringValueExpression COMMA stringLength RIGHT_PAREN
   ;

byteStringTrimFunction
   : TRIM LEFT_PAREN byteStringTrimOperands RIGHT_PAREN
   ;

byteStringTrimOperands
   : (trimSpecification? trimByteString? FROM)? byteStringTrimSource
   ;

byteStringTrimSource
   : byteStringValueExpression
   ;

trimByteString
   : byteStringValueExpression
   ;

datetimeValueExpression
   : datetimePrimary
   | durationValueExpression PLUS_SIGN datetimePrimary
   | datetimeValueExpression PLUS_SIGN durationTerm
   | datetimeValueExpression MINUS_SIGN durationTerm
   ;

datetimePrimary
   : valueExpressionPrimary
   | datetimeValueFunction
   ;

datetimeValueFunction
   : dateFunction
   | timeFunction
   | datetimeFunction
   | localtimeFunction
   | localdatetimeFunction
   ;

dateFunction
   : CURRENT_DATE
   | DATE LEFT_PAREN dateFunctionParameters? RIGHT_PAREN
   ;

timeFunction
   : CURRENT_TIME
   | ZONED_TIME LEFT_PAREN timeFunctionParameters? RIGHT_PAREN
   ;

localtimeFunction
   : LOCAL_TIME (LEFT_PAREN timeFunctionParameters? RIGHT_PAREN)?
   ;

datetimeFunction
   : CURRENT_TIMESTAMP
   | ZONED_DATETIME LEFT_PAREN datetimeFunctionParameters? RIGHT_PAREN
   ;

localdatetimeFunction
   : LOCAL_TIMESTAMP
   | LOCAL_DATETIME LEFT_PAREN datetimeFunctionParameters? RIGHT_PAREN
   ;

dateFunctionParameters
   : DATE_STRING
   | recordConstructor
   ;

timeFunctionParameters
   : TIME_STRING
   | recordConstructor
   ;

datetimeFunctionParameters
   : DATETIME_STRING
   | recordConstructor
   ;

durationValueExpression
   : durationTerm
   | durationAdditionAndSubtraction
   | datetimeSubtraction
   ;

durationAdditionAndSubtraction
   : durationValueExpression1 PLUS_SIGN durationTerm1
   | durationValueExpression1 MINUS_SIGN durationTerm1
   ;

datetimeSubtraction
   : DURATION_BETWEEN LEFT_PAREN datetimeSubtractionParameters RIGHT_PAREN temporalDurationQualifier?
   ;

datetimeSubtractionParameters
   : datetimeValueExpression1 COMMA datetimeValueExpression2
   ;

durationTerm
   : durationFactor
   | durationTerm2 ASTERISK factor
   | durationTerm2 SOLIDUS factor
   | term ASTERISK durationFactor
   ;

durationFactor
   : SIGN? durationPrimary
   ;

durationPrimary
   : valueExpressionPrimary
   | durationValueFunction
   ;

durationValueExpression1
   : durationValueExpression
   ;

durationTerm1
   : durationTerm
   ;

durationTerm2
   : durationTerm
   ;

datetimeValueExpression1
   : datetimeValueExpression
   ;

datetimeValueExpression2
   : datetimeValueExpression
   ;

durationValueFunction
   : durationFunction
   | durationAbsoluteValueFunction
   ;

durationFunction
   : DURATION LEFT_PAREN durationFunctionParameters RIGHT_PAREN
   ;

durationFunctionParameters
   : DURATION_STRING
   | recordConstructor
   ;

durationAbsoluteValueFunction
   : ABS LEFT_PAREN durationValueExpression RIGHT_PAREN
   ;

authorizationIdentifier
   : IDENTIFIER
   ;

objectName
   : IDENTIFIER
   ;

objectNameOrBindingVariable
   : REGULAR_IDENTIFIER
   ;

directoryName
   : IDENTIFIER
   ;

schemaName
   : IDENTIFIER
   ;

graphName
   : REGULAR_IDENTIFIER
   | delimitedGraphName
   ;

delimitedGraphName
   : DELIMITED_IDENTIFIER
   ;

graphTypeName
   : IDENTIFIER
   ;

nodeTypeName
   : IDENTIFIER
   ;

edgeTypeName
   : IDENTIFIER
   ;

bindingTableName
   : REGULAR_IDENTIFIER
   | delimitedBindingTableName
   ;

delimitedBindingTableName
   : DELIMITED_IDENTIFIER
   ;

procedureName
   : IDENTIFIER
   ;

labelName
   : IDENTIFIER
   ;

propertyName
   : IDENTIFIER
   ;

fieldName
   : IDENTIFIER
   ;

parameterName
   : SEPARATED_IDENTIFIER
   ;

graphPatternVariable
   : elementVariable
   | pathOrSubpathVariable
   ;

pathOrSubpathVariable
   : pathVariable
   | subpathVariable
   ;

elementVariable
   : bindingVariable
   ;

pathVariable
   : bindingVariable
   ;

subpathVariable
   : REGULAR_IDENTIFIER
   ;

bindingVariable
   : REGULAR_IDENTIFIER
   ;

literal
   : SIGNED_NUMERIC_LITERAL
   | generalLiteral
   ;

unsignedLiteral
   : UNSIGNED_NUMERIC_LITERAL
   | generalLiteral
   ;

generalLiteral
   : BOOLEAN_LITERAL
   | CHARACTER_STRING_LITERAL
   | BYTE_STRING_LITERAL
   | TEMPORAL_LITERAL
   | DURATION_LITERAL
   | NULL_LITERAL
   | listLiteral
   | recordLiteral
   ;

listLiteral
   : listValueConstructorByEnumeration
   ;

recordLiteral
   : recordConstructor
   ;



