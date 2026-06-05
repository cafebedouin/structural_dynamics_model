% ============================================================================
% CONSTRAINT STORY: retroactive_criminalization_of_coerced_acts
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_retroactive_criminalization_of_coerced_acts, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: retroactive_criminalization_of_coerced_acts
 *   human_readable: Retroactive Criminalization of Institutionally Coerced Acts
 *   domain: labor_relations/institutional_control/debt_bondage
 *
 * SUMMARY:
 *   Retroactive criminalization of institutionally coerced acts is a control
 *   mechanism in which institutional agents induce prohibited behavior
 *   through authority directives, then prosecute the induced behavior as
 *   voluntary contract violation. The constraint operates through a
 *   three-stage process: (1) recruitment via debt leverage or false
 *   solidarity, (2) operational directives that require contract violations
 *   (unauthorized disclosures, regulatory breaches, prohibited associations),
 *   and (3) retroactive enforcement that frames the coerced acts as voluntary
 *   breaches when the informant becomes inconvenient or the operation
 *   concludes. The primary observable is the gap between institutional
 *   encouragement documented in case files and acknowledgment of that
 *   encouragement in tribunal proceedings. Contract liability enforcement
 *   rates differ systematically: intelligence-operation participants face
 *   prosecution at rates 4-6x higher than organic violators with equivalent
 *   breach severity, but tribunal records attribute liability to voluntary
 *   choice rather than institutional coercion. The constraint's theater ratio
 *   (0.65) reflects that tribunal proceedings perform voluntary-breach
 *   adjudication while systematically excluding evidence of institutional
 *   directives. The extractiveness trajectory shows accumulation over the
 *   15-year interval: initial recruitment focused on genuine operational
 *   necessity (ε=0.62), but as the mechanism became institutionalized,
 *   enforcement shifted toward liability externalization and control (ε=0.78
 *   plateau).
 *
 * KEY AGENTS:
 *   - Coerced Informants: Primary victims (powerless/trapped) — induced to violate contracts through institutional directives, then prosecuted for voluntary breach; debt bondage prevents exit and institutional records erase coercion evidence
 *   - Debt-Bonded Workers: Secondary victims (moderate/constrained) — economic dependency creates functional entrapment; induced to perform prohibited acts through supervisor directives, then held individually liable
 *   - Contract Enforcement Division: Primary beneficiary (institutional/arbitrage) — captures deterrence and control benefits while externalizing liability; selective enforcement based on institutional convenience
 *   - Operational Handlers: Mixed position (institutional/constrained) — benefit from operational flexibility but bear reputational risk if the pattern becomes publicly legible
 *   - Labor Rights Coalition: Organized victims (organized/constrained) — attempting to document the pattern but suppressed by evidentiary barriers and sealed proceedings
 *   - Independent Tribunal Adjudicators: Mixed position (powerful/mobile) — benefit from clear liability rules but recognize extraction mechanism when case files reveal encouragement patterns; institutional pressure suppresses refusal to enforce
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function (operational security) coexisting with severe extraction (liability externalization onto coerced actors)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(retroactive_criminalization_of_coerced_acts, 0.78).
domain_priors:suppression_score(retroactive_criminalization_of_coerced_acts, 0.88).
domain_priors:theater_ratio(retroactive_criminalization_of_coerced_acts, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(retroactive_criminalization_of_coerced_acts, extractiveness, 0.78).
narrative_ontology:constraint_metric(retroactive_criminalization_of_coerced_acts, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(retroactive_criminalization_of_coerced_acts, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(retroactive_criminalization_of_coerced_acts, snare).
narrative_ontology:human_readable(retroactive_criminalization_of_coerced_acts, "Retroactive Criminalization of Institutionally Coerced Acts").
narrative_ontology:topic_domain(retroactive_criminalization_of_coerced_acts, "labor_relations/institutional_control/debt_bondage").

domain_priors:requires_active_enforcement(retroactive_criminalization_of_coerced_acts).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(retroactive_criminalization_of_coerced_acts, contract_enforcement_division).
narrative_ontology:constraint_beneficiary(retroactive_criminalization_of_coerced_acts, institutional_authority_structure).
narrative_ontology:constraint_victim(retroactive_criminalization_of_coerced_acts, coerced_informants).
narrative_ontology:constraint_victim(retroactive_criminalization_of_coerced_acts, debt_bonded_workers).
narrative_ontology:constraint_victim(retroactive_criminalization_of_coerced_acts, authority_directed_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED INFORMANT (SNARE) — Trapped by debt leverage and authority directives. Induced to violate contract terms through institutional encouragement, then prosecuted for voluntary breach when the operation concludes or the informant becomes inconvenient. No exit: debt bondage prevents walking away, and the institutional record erases evidence of coercion. Maximum extraction: bears criminal liability for acts the institution directed.
constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEBT-BONDED WORKER (SNARE) — Constrained by economic dependency but not fully trapped. Can theoretically exit through debt repayment or alternative employment, but the constraint's design ensures exit costs exceed capacity. Induced to perform prohibited acts (unauthorized disclosures, contract violations, regulatory breaches) through supervisor directives framed as operational necessity, then held individually liable when the institutional benefit is captured. High extraction despite moderate power: the constraint converts constrained exit into functional entrapment through retroactive liability.
constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTRACT ENFORCEMENT DIVISION (ROPE) — Primary beneficiary. Experiences the constraint as coordination: enforcing contract terms maintains institutional discipline and operational security. The division captures the benefit (deterrence, control, liability externalization) while bearing none of the cost. Arbitrage exit: can selectively enforce or waive liability based on institutional convenience. Negative effective extraction: the constraint subsidizes this agent's authority.
constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: OPERATIONAL HANDLER (TANGLED ROPE) — Institutional actor with constrained exit. Benefits from the constraint's control mechanism (can direct informants to violate contracts for operational gain) but also bears risk if the retroactive criminalization pattern becomes publicly legible and damages institutional legitimacy. Mixed extraction: coordination function (operational flexibility) coexists with extraction (liability externalization onto informants). Constrained exit: cannot unilaterally change enforcement policy but can advocate internally.
constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR RIGHTS COALITION (SNARE) — Organized agents (legal aid groups, worker advocacy organizations, investigative journalists) attempting to document the pattern. Constrained by evidentiary barriers: institutional records of encouragement are classified, destroyed, or framed as misinterpretation. The coalition has organizational capacity but faces suppression through legal barriers (state secrets privilege, sealed tribunal proceedings, NDAs). High extraction: the constraint's theater (framing coerced acts as voluntary breaches) actively obstructs the coalition's documentation efforts.
constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INDEPENDENT TRIBUNAL ADJUDICATOR (TANGLED ROPE) — Powerful agent with mobile exit (can resign, transfer to private practice, or refuse cases). Benefits from the constraint's coordination function (clear liability rules enable efficient adjudication) but also recognizes the extraction mechanism when case files reveal institutional encouragement patterns. Mixed experience: the constraint enables their role (adjudicating contract disputes) while creating moral hazard (punishing coerced actors for institutional directives). Mobile exit means they could refuse to enforce, but institutional pressure and career incentives suppress that option.
constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (contract enforcement maintains institutional discipline and operational security) and the asymmetric extraction (liability is externalized onto coerced actors while institutional authority captures the benefit). The constraint is not pure extraction: there is a real coordination problem (how to maintain operational security when informants have access to sensitive information). But the solution embeds severe extraction: the institution induces prohibited behavior, then prosecutes it as voluntary breach. Tangled Rope classification reflects this duality.
constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(retroactive_criminalization_of_coerced_acts_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(retroactive_criminalization_of_coerced_acts, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(retroactive_criminalization_of_coerced_acts, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(retroactive_criminalization_of_coerced_acts_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. The constraint extracts from coerced informants and debt-bonded workers by converting institutional directives into individual criminal liability. The institution captures the operational benefit (intelligence gathering, labor compliance, operational flexibility) while externalizing the legal and reputational cost onto the coerced actor. The extractiveness is not maximal (not 0.85+) because some informants do receive compensation, protection, or debt forgiveness — the extraction is severe but not total. The trajectory shows accumulation: initial recruitment (T=0, ε=0.62) focused on operational necessity, but as the mechanism became institutionalized, enforcement shifted toward control and liability externalization (T=9-15, ε=0.78 plateau). Suppression (0.88): Very high. Multiple suppression mechanisms operate simultaneously: (1) debt bondage prevents exit, (2) institutional records of encouragement are classified or destroyed, (3) tribunal proceedings exclude coercion evidence through evidentiary rules (state secrets privilege, hearsay exclusions, sealed proceedings), (4) NDAs and retaliation threats suppress whistleblowing, (5) the framing of coerced acts as voluntary breaches isolates victims from legal aid and public sympathy. Suppression is not maximal (not 0.95+) because some cases do surface through investigative journalism or legal aid organizations, and some adjudicators do recognize the pattern even if they cannot formally acknowledge it. Theater ratio (0.65): High. Tribunal proceedings perform voluntary-breach adjudication (evaluating intent, contract interpretation, liability allocation) while systematically excluding the evidence that would reveal institutional coercion. The theater is not maximal because some functional adjudication occurs (distinguishing breach severity, assessing damages, applying contract law), but the core function (determining whether the breach was voluntary or coerced) is performative. The theater ratio increased over the interval as the evidentiary exclusions became more systematic and the framing of voluntary breach became more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a characteristic snare pattern: victims see pure extraction (snare), beneficiaries see coordination (rope), and mixed-position agents see tangled rope. The coerced informant sees a snare: induced to violate contracts through institutional directives, then prosecuted for voluntary breach with no exit and no acknowledgment of coercion. The contract enforcement division sees a rope: enforcing contract terms maintains institutional discipline and operational security, with no experienced cost. The operational handler sees a tangled rope: operational flexibility (coordination) coexists with liability externalization (extraction). The independent adjudicator sees a tangled rope: clear liability rules (coordination) coexist with moral hazard (punishing coerced actors for institutional directives). The labor rights coalition sees a snare: their documentation efforts are actively suppressed by evidentiary barriers and sealed proceedings. The analytical observer sees a tangled rope: genuine coordination function (operational security) coexists with severe extraction (retroactive criminalization of coerced acts). The perspectival gap is not a disagreement about facts — all agents have access to the same structural data (institutional directives exist, prosecutions occur, tribunal proceedings exclude coercion evidence). The gap reflects different structural positions: who benefits, who bears costs, and what exit options exist. The constraint's theater (framing coerced acts as voluntary breaches) actively maintains the perspectival gap by preventing victims from making their coercion legible in formal proceedings.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality structure reflects a clear beneficiary-victim divide with institutional arbitrage. Coerced informants (powerless/trapped) are full victims: they bear the legal liability for acts the institution directed, with no exit option and no compensation. The engine derives d ≈ 0.95 (full target) from victim status + trapped exit, producing maximum effective extraction. Debt-bonded workers (moderate/constrained) are also victims but with slightly lower d ≈ 0.85 due to constrained rather than trapped exit — they could theoretically exit through debt repayment, though the constraint's design makes this functionally impossible. The contract enforcement division (institutional/arbitrage) is the primary beneficiary: captures control and deterrence benefits while bearing no cost. The engine derives d ≈ 0.05 (full beneficiary) from beneficiary status + arbitrage exit, producing negative effective extraction (the constraint subsidizes this agent). Operational handlers (institutional/constrained) occupy a mixed position: they benefit from operational flexibility but bear reputational risk. The engine derives d ≈ 0.35 from beneficiary status + constrained exit, producing low but positive extraction. The labor rights coalition (organized/constrained) is a victim of the constraint's suppression mechanism: their documentation efforts are actively obstructed. The engine derives d ≈ 0.60 from victim status + organized power + constrained exit. Independent adjudicators (powerful/mobile) occupy a mixed position: they benefit from clear liability rules but recognize the extraction mechanism. The engine derives d ≈ 0.45 from mixed beneficiary-victim status + mobile exit, producing moderate extraction. The analytical observer sees the full structure: genuine coordination function (operational security) coexisting with severe extraction (liability externalization). No directionality override is needed — the structural derivation from beneficiary/victim declarations + exit options produces accurate d values for all agents.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is classified as snare rather than tangled_rope because the coordination function (operational security) does not justify the extraction mechanism (retroactive criminalization of coerced acts). The mandatrophy question is: does the operational security benefit require prosecuting coerced informants for acts the institution directed, or could the security benefit be achieved through alternative mechanisms (compartmentalization, technical controls, institutional liability acceptance)? The analytical perspective acknowledges the genuine coordination function but classifies as tangled_rope rather than rope because the extraction is severe and asymmetric. The primary victims (coerced informants, debt-bonded workers) classify as snare because they experience pure extraction with no coordination benefit. The beneficiary (contract enforcement division) classifies as rope because they experience pure coordination with no cost. The perspectival gap is resolved by recognizing that the coordination function is real but does not require the extraction mechanism — the constraint is a snare with a coordination cover story. The omega variable 'operational_necessity_vs_liability_externalization' directly addresses the mandatrophy: if operational necessity is genuine, the constraint is tangled_rope with high extraction; if operational necessity is post-hoc rationalization, the constraint is pure snare. The empirical resolution mechanism is enforcement pattern analysis: do prosecutions correlate with operational security breaches or with institutional convenience? Preliminary data suggests the latter, supporting the snare classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_encouragement_legibility,
    'What evidentiary standard would make institutional encouragement legible in tribunal proceedings?',
    'Comparative analysis of case files with institutional encouragement documentation vs tribunal acknowledgment rates; identification of evidentiary thresholds that shift liability attribution from individual to institutional',
    'If standard is achievable: constraint shifts toward tangled_rope (coordination with extractive overhead). If standard is systematically unattainable: constraint remains snare (extraction mechanism disguised as voluntary breach).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_encouragement_legibility, empirical, 'Evidentiary threshold for proving institutional coercion in tribunal proceedings').

omega_variable(
    operational_necessity_vs_liability_externalization,
    'Does the constraint serve a genuine operational security function, or is operational necessity a post-hoc rationalization for liability externalization?',
    'Longitudinal analysis of enforcement patterns: do prosecutions correlate with operational security breaches or with institutional convenience (informant becomes inconvenient, operation concludes, budget pressures)? Comparison with alternative security mechanisms (compartmentalization, technical controls, institutional liability acceptance).',
    'If operational necessity is genuine: coordination function is real, and the constraint is tangled_rope with high but not pure extraction. If operational necessity is post-hoc: coordination function is theater, and the constraint is pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_necessity_vs_liability_externalization, empirical, 'Whether operational security justification is genuine or post-hoc rationalization').

omega_variable(
    debt_leverage_necessity,
    'Is debt leverage a necessary precondition for the retroactive criminalization mechanism, or does the mechanism operate independently?',
    'Case analysis: prosecution rate for debt-bonded informants vs non-debt-bonded informants who received institutional directives. If debt leverage is necessary, the constraint is downstream of debt_leverage_as_consent_manufacturing. If the mechanism operates independently, the constraints are parallel.',
    'If debt leverage is necessary: the constraint''s extraction is amplified by upstream debt bondage, and the network relationship is causal dependency. If independent: the constraint operates through authority directives alone, and debt leverage is one of multiple recruitment mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_leverage_necessity, empirical, 'Whether debt leverage is necessary for the retroactive criminalization mechanism').

omega_variable(
    selective_enforcement_pattern,
    'Is enforcement selective based on institutional convenience, or does it follow consistent criteria?',
    'Statistical analysis of enforcement decisions: correlation between prosecution and (a) operational outcome (success/failure), (b) informant compliance with subsequent directives, (c) institutional budget cycles, (d) public visibility of the case. Consistent criteria would show low correlation with convenience factors; selective enforcement would show high correlation.',
    'If selective: the constraint is a control mechanism (snare classification confirmed). If consistent: the constraint is a liability rule with high but predictable costs (shifts toward tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_enforcement_pattern, empirical, 'Whether enforcement is selective or follows consistent criteria').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(retroactive_criminalization_of_coerced_acts, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(retro_crim_tr_t0, retroactive_criminalization_of_coerced_acts, theater_ratio, 0, 0.45).
narrative_ontology:measurement(retro_crim_tr_t3, retroactive_criminalization_of_coerced_acts, theater_ratio, 3, 0.52).
narrative_ontology:measurement(retro_crim_tr_t6, retroactive_criminalization_of_coerced_acts, theater_ratio, 6, 0.58).
narrative_ontology:measurement(retro_crim_tr_t9, retroactive_criminalization_of_coerced_acts, theater_ratio, 9, 0.62).
narrative_ontology:measurement(retro_crim_tr_t12, retroactive_criminalization_of_coerced_acts, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(retro_crim_be_t0, retroactive_criminalization_of_coerced_acts, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(retro_crim_be_t3, retroactive_criminalization_of_coerced_acts, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(retro_crim_be_t6, retroactive_criminalization_of_coerced_acts, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(retro_crim_be_t9, retroactive_criminalization_of_coerced_acts, base_extractiveness, 9, 0.75).
narrative_ontology:measurement(retro_crim_be_t12, retroactive_criminalization_of_coerced_acts, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(retro_crim_be_t15, retroactive_criminalization_of_coerced_acts, base_extractiveness, 15, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(retroactive_criminalization_of_coerced_acts, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of two recruitment mechanisms: debt_leverage_as_consent_manufacturing (which creates the trapped exit condition) and informant_recruitment_through_false_solidarity (which creates the initial trust relationship that enables institutional directives). The retroactive criminalization mechanism operates on top of these recruitment constraints, converting the coerced relationship into individual criminal liability. The three constraints form a pipeline: recruitment → operational directives → retroactive enforcement. Each has its own extractiveness value reflecting its distinct structural mechanism, but they are causally linked in the institutional control architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
