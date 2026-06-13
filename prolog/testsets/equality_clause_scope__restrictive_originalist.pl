% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Equality Clause (Restrictive Originalist Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   The equality clause of the U.S. Constitution (1787) states 'all men are
 *   created equal' in the Declaration and 'equal protection' in the 14th
 *   Amendment (1868). This constraint represents ONE READING of that
 *   contested kernel: the restrictive originalist reading holds that
 *   'equality' applies only to propertied white male citizens competing
 *   within the political actor set the framers recognized. Claims from
 *   enslaved persons, women, indigenous peoples, and the non-propertied rest
 *   on a different constitutional basis (subsequent amendments, not the
 *   equality principle itself). The constraint coordinates the beneficiary
 *   set's intra-class competition while actively suppressing (through
 *   doctrine and enforcement) alternative readings that would extend equality
 *   across racial, gender, and property lines. Theater ratio rises over the
 *   interval as the constraint's justifications become increasingly
 *   elaborate—appeals to framers' intent, original meaning, amendment
 *   procedures—to defend an exclusionary scope that resistance pressures from
 *   all four excluded-group levels.
 *
 * KEY AGENTS:
 *   - Propertied white male citizens: beneficiaries of equality clause within their own set; control interpretation through federal courts
 *   - Enslaved persons: powerless targets, explicitly excluded from personhood and thus from equality's scope
 *   - Women: constrained targets, excluded by coverture and property law; forced to seek separate amendment (14th, 19th)
 *   - Indigenous peoples: excluded as external sovereigns, not constituent population; require amendment for inclusion
 *   - Non-propertied males: excluded by property requirements; equality clause does not protect their franchise claims
 *   - Free blacks: ambiguously positioned; nominally free but racially barred from franchise and legal standing
 *   - Federal courts: agenda-setters enforcing original meaning; resist expansionist interpretation
 *   - State legislatures: agenda-setters defining franchise and property law within equal-protection bounds (narrowly interpreted)
 *   - Expansionist interpreters: excluded from the interpretive process; their reading forecloses or rivals this one
 *   - Analytical observer: perceives the constraint's full architecture and its cumulative extraction from all excluded groups
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.78).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.81).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.78).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Equality Clause (Restrictive Originalist Reading)").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, '64651cc0-8bcf-4d1b-ad75-4dd196d61b80').
narrative_ontology:cs_kernel_codification('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', fixed_text).
narrative_ontology:cs_authority_grounding('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', lineage).
narrative_ontology:cs_interpretation_layer_present('64651cc0-8bcf-4d1b-ad75-4dd196d61b80').
narrative_ontology:cs_reading_relation('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', foundational, equality_applies_political_actors_only).
narrative_ontology:cs_axiom_status(equality_applies_political_actors_only, holdable).
narrative_ontology:cs_axiom_grounding('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', equality_applies_political_actors_only, conventional).
narrative_ontology:cs_axiom('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', foundational, original_public_meaning_binding).
narrative_ontology:cs_axiom_status(original_public_meaning_binding, overridden).
narrative_ontology:cs_axiom_grounding('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', original_public_meaning_binding, empirically_contingent).
narrative_ontology:cs_reference_frame('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', framers_intent_intra_class_equality).
narrative_ontology:cs_drift_state('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', eighteen_sixty_eight_civil_rights_amendment, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('64651cc0-8bcf-4d1b-ad75-4dd196d61b80', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, indigenous_peoples).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, non_propertied_males).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_blacks).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, framers_intent_supremacy).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, amendment_requirement_for_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess franchise, property rights, and legal standing within the constitutional order. The equality clause, read under this framework, protects their competitive access to political power and economic participation against arbitrary state discrimination. They do not face expansion claims against their interests and control the interpretation machinery through the judiciary.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens, beneficiary,
    powerful, generational, arbitrage, national).

% Are held as property, not persons under the constitution. Equality clause offers no protection because they are not recognized as subjects of rights. Their exclusion is the enforcement object itself—the reading depends on treating them as non-citizens to whom the equality principle does not apply.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_persons, payer,
    powerless, immediate, trapped, national).

% Are excluded from franchise and property control by coverture law. The equality clause, under this reading, does not extend to gender-based classifications in civic participation because women were understood to be outside the political actor category the framers addressed. Their claims require a separate amendment (14th, 19th).
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women, payer,
    powerless, biographical, identity_locked, national).

% Are treated as external sovereigns, not constituent population. Equality clause applies only to those within the political community; indigenous peoples are deemed outside that scope in the 18th-century framework. Their inclusion requires constitutional amendment and political recognition, not judicial extension of equality doctrine.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, indigenous_peoples, payer,
    powerless, civilizational, trapped, national).

% Are excluded from franchise due to property requirements. The equality clause does not protect them against wealth-based franchise restrictions because 'equal' applies only to those already within the political actor set. They bear the cost of disenfranchisement without a doctrinal path to remedy short of amendment.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, non_propertied_males, payer,
    moderate, biographical, constrained, national).

% Occupy an ambiguous status: some possess property and nominal freedom, but are barred from franchise and legal standing by race-based statutes. The equality clause, under this reading, applies only to competition within the white male citizen set and does not require states to extend franchise across racial lines.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_blacks, payer,
    moderate, biographical, constrained, national).

% Retain authority to define the bounds of franchise, property, and civic participation within the equality clause's boundaries. They may discriminate on grounds the framers did not contemplate as constitutional violations because the clause applies only within the 18th-century scope. Amendment is required to expand that scope.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, state_legislatures, agenda_setter,
    institutional, generational, mobile, national).

% Interpret the equality clause according to original public meaning and framers' intent. Their role is to enforce the constraint as originally understood, not to expand it to classes the framers excluded. Judicial restraint is the norm; expansion requires constitutional amendment, not reinterpretation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, federal_courts, agenda_setter,
    institutional, generational, mobile, national).

% Argue that the equality principle's text contains latent universalist content that courts may recognize and apply to historically excluded groups. Under the restrictive originalist reading, they are shut out from the judicial interpretation process because their framework contradicts the binding commitment to original meaning.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, expansionist_interpreters, excluded,
    institutional, generational, trapped, national).

% Advocate for constitutional recognition of enslaved persons as subjects of rights and equality. The restrictive originalist reading structurally bars their claims: they must seek constitutional amendment because the original equality clause does not encompass slavery as a violation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, abolitionist_reformers, excluded,
    organized, biographical, constrained, national).

% Observes the constraint's structural operation: how the equality clause, read through the originalist lens, coordinates the political actor set (propertied white males) while suppressing competing interpretations and barring excluded groups from claiming constitutional protection for their own equality.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_citizens).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, rule-bound framework for competition and political participation among propertied white male citizens. The equality clause prevents arbitrary state discrimination within that set: a merchant may not be arbitrarily excluded from trade or political voice compared to his peer. It coordinates interstate citizenship and commerce among the enfranchised set without requiring expansion of that set.
% TRANSFER_FUNCTION: Moves legitimacy from excluded groups' claims to constitutional protection. The constraint transfers the burden of proof: those outside the original scope (women, enslaved persons, indigenous peoples, non-propertied males) must obtain a separate constitutional amendment to claim equality rights, while the enfranchised set retains constitutional equality as a baseline. The benefit accrues to those already within the political community.
% ABSENT_VOICES: Enslaved persons, women, and indigenous peoples are structurally excluded from the political conversation the constraint addresses. They would argue that equality is a universal principle that applies to all humans and that the framers' exclusions reflect power, not principle. Free blacks and non-propertied males, though nominally present, lack standing in the framework and cannot claim constitutional remedy for their exclusion.
% DISAPPEARANCE_RATIONALE: If this reading of the equality clause and its original-meaning constraint disappeared, the entire constitutional architecture governing franchise, citizenship, and rights claims would reorganize. States would lose a legitimizing doctrine for exclusion and would face immediate pressure to justify franchise restrictions and property-based discrimination on grounds other than 'original meaning.' The constraint's collapse would open judicial pathways to expansionist interpretation.
% FOUNDING_PROBLEM: Need for a stable, judicially enforceable rule against arbitrary state discrimination within the propertied, enfranchised male citizen set. The founding problem is INTRA-CLASS stability: preventing one propertied white male citizen from being arbitrarily disadvantaged relative to his peers in commerce, property, or political voice.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative jurists attest the founding problem remains live—protecting against arbitrary intra-class discrimination is an enduring constitutional function. Progressive legal scholars and historians attest the problem is solved and the constraint persists as a doctrinal gate against expansion; external corroboration from historical scholarship shows the framers' intent was indeed intra-class stabilization, NOT universal equality application (Foner, McPherson, Rakove on framers' exclusionary intent).
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness and suppression both rise substantially from 1787 to 1868 as the constraint faces mounting pressure from abolitionism, women's rights, and democratic expansion. The founding extraction (0.71) reflects the original exclusion embedded in the text; by 1868 (0.78) the constraint has hardened as it resists the civil-war-era movements for inclusion. Theater ratio grows from 0.25 to 0.42 because the justification increasingly relies on doctrinal apparatus (original meaning, framers' intent, amendment requirements) rather than immediate enforcement—the constraint must be defended argumentatively as resistance rises. Suppression requirement peaks (0.81) because the constraint's persistence depends on actively excluding alternative interpretations from judicial consideration, not merely on initial-set design. The coercion grid's level-differentiated dynamics show that structural suppression is highest (0.85 at 1868) while individual resistance is lowest (0.55), capturing how the constraint operates through constitutional doctrine and institutional design rather than individual-level force. Resistance at the class and organizational levels rises sharply (0.72, 0.68) as abolitionists and women's rights advocates mount sustained campaigns; the constraint survives only because structural authority (courts, amendment procedure, federal structure) remains loyal to the restrictive reading.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary (propertied white male) seat, the constraint appears as a stabilizing rule protecting competitive equality within a natural political community. From the victim seats (especially enslaved persons and women), the same constraint is pure extraction: the text's universalist language is invoked then denied, and alternatives are barred not by logical necessity but by institutional power. The federal courts, as interpreters, occupy an intermediate position—they see themselves as neutral custodians of original meaning, but that 'neutrality' systematically protects the beneficiary set by refusing to recognize excluded groups' claims as constitutional questions. The analytical observer perceives the constraint as a tangled rope: it coordinates the beneficiary set's internal political life (genuine coordination function) while actively suppressing excluded groups' exit pathways and alternative readings (asymmetric extraction). This perspectival divergence is the constraint's deepest structural property.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied white male citizens occupy d ~ 0.1–0.2 (near full beneficiary): they collect political equality within their set, control the interpretation machinery, and face no suppression. Enslaved persons occupy d ~ 0.95 (near full target): they are excluded from the constraint's scope entirely and bear the costs of exclusion without benefit or voice. Women occupy d ~ 0.85 (high target): excluded by coverture, bearing identity-lock suppression (legal dependency on male family members), forced to seek external amendment. Non-propertied males occupy d ~ 0.75 (target with mobility): they can eventually gain property and enter the beneficiary set; their exit is constrained but not trapped. Free blacks occupy d ~ 0.80 (target with organizational capacity): free blacks can organize and petition, creating modest resistance; they are constrained rather than powerless, but the constraint's interpretation explicitly excludes them. Expansionist interpreters are not stakeholders in the traditional sense—they are excluded from the framework itself—but if modeled structurally, they would occupy d ~ 1.0 within the constraint's logic (shut out entirely from interpretation authority). The directionality distribution is extreme: 0.1–0.2 for beneficiaries, 0.75–0.95 for victims, reflecting how thoroughly the constraint's benefits concentrate and harms distribute.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive originalist reading does NOT exhibit mandatrophy in the sense of an atrophied founding function. The founding problem (intra-class equality among propertied white males) remains live throughout the interval—this reading genuinely serves coordination for its beneficiary set. However, the reading DOES exhibit a growing MANDATE INFLATION: as resistance pressures mount, the constraint expands its justificatory apparatus and enforcement machinery to defend an increasingly indefensible exclusionary scope. By 1868, the constraint's persistence depends less on the coordination function (which is robust) and more on doctrinal gatekeeping (amendment procedures, original-meaning deference, federalism). This is not piton-level deterioration (the machinery is well-maintained), but it is a drift toward extraction-dominated operation. The suppression requirement grows faster than extractiveness itself, suggesting the constraint is increasingly maintained by active suppression of alternatives rather than by voluntary participant agreement. Mandatrophy is not present, but mandatrophy-adjacent dynamics (inflation of justification, narrowing of consent base) characterize the later interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universalist_latency_in_text,
    'Does the Declaration''s ''all men are created equal'' contain latent universalist content that the framers chose to ignore, or is the text genuinely narrower than later interpreters claim?',
    'Textual analysis against founding-era usage and comparable documents; historical scholarship on framers'' explicit statements about equality''s scope; comparative constitutional analysis of how equivalent language was understood contemporaneously.',
    'If latency is established, the expansive universalist reading gains textual grounding and the restrictive originalist reading loses its monopoly on ''original meaning.'' If the text is genuinely narrow, the restrictive reading holds and expansion requires amendment, not reinterpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universalist_latency_in_text, empirical, 'Whether the equality principle''s text contains suppressed universalism or is authentically narrow.').

omega_variable(
    identity_lock_mechanism_gender,
    'Is the suppression of women''s equality claims under this reading structural (legal barriers, property law) or internalized (women believing coverture reflects natural order), and does the distinction affect the constraint''s classification?',
    'Post-amendment behavioral change: if women''s resistance to the constraint persists after legal barriers fall (14th/19th amendments), the suppression was partially internalized. If resistance collapses immediately upon legal opening, suppression was structural.',
    'Fully internalized suppression would suggest the constraint''s effective suppression exceeds the measured 0.81 because targets carry suppression with them after exit. Structural suppression would keep the measured value as the effective extraction. The distinction affects whether piton-like degradation occurs post-amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_gender, empirical, 'Structural vs. internalized suppression mechanism in gender-based exclusion.').

omega_variable(
    amendment_procedure_gatekeeping,
    'Is the requirement for constitutional amendment (rather than judicial reinterpretation) a neutral procedural boundary, or does it systematically favor the status quo by making expansion prohibitively costly?',
    'Comparative analysis: how easily do other constitutions (without amendment requirements) extend equality protections? How much mobilization effort does U.S. amendment require compared to judicial reinterpretation in other systems?',
    'If amendment is systematically harder than reinterpretation, the amendment requirement is itself an extraction mechanism—part of the constraint''s suppression machinery rather than a neutral structural feature. If neutral, the restrictive originalist reading is procedurally justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_procedure_gatekeeping, conceptual, 'Whether the amendment procedure is a neutral constraint or a systematic favor for exclusionary readings.').

omega_variable(
    framing_naturalness_of_beneficiary_set,
    'Does the ''propertied white male citizen'' category represent a natural political-actor boundary or a constructed category that the reading naturalizes through originalism?',
    'Historical counterfactual: if the framers had been forced to articulate why this set is the ''natural'' political actor set, what justification would they offer? Would that justification hold up against universalist challenges in their own terms?',
    'If the category is revealed as constructed (chosen for contingent power reasons, not natural law), the restrictive originalist reading loses its claim to be discovering ''original meaning'' and becomes a choice to enforce a particular reading. If the category is defensible on its own terms (even if exclusionary), the reading retains some justificatory grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_naturalness_of_beneficiary_set, conceptual, 'Whether the beneficiary set reflects an objective political boundary or a power-constructed category defended as ''natural'' by originalism.').

omega_variable(
    covenant_vs_contract_reading_contest,
    'Is the equality clause best read as part of a ''social contract'' binding ''the people'' as a unified body (expansive reading), or as a contract among pre-existing political actors (restrictive reading)?',
    'Textual archaeology: which metaphors (covenant, contract, social compact, constitution) do the founding documents use? Which metaphor dominated contemporary discourse? Did usage shift between 1776 and 1789?',
    'A contract-among-actors reading supports restrictive originalism; a covenant-of-the-people reading supports expansive universalism. This is a conceptual omega because the choice of political metaphor sits beneath the empirical questions and determines what counts as ''original meaning.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_vs_contract_reading_contest, conceptual, 'Metaphorical framing of the constitutional relationship (contract vs. covenant) determines what ''original meaning'' includes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1787, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1787, equality_clause_scope__restrictive_originalist, theater_ratio, 1787, 0.25).
narrative_ontology:measurement_basis(equa_tr_t1787, observed).
narrative_ontology:measurement(equa_tr_t1808, equality_clause_scope__restrictive_originalist, theater_ratio, 1808, 0.28).
narrative_ontology:measurement_basis(equa_tr_t1808, observed).
narrative_ontology:measurement(equa_tr_t1830, equality_clause_scope__restrictive_originalist, theater_ratio, 1830, 0.35).
narrative_ontology:measurement_basis(equa_tr_t1830, observed).
narrative_ontology:measurement(equa_tr_t1850, equality_clause_scope__restrictive_originalist, theater_ratio, 1850, 0.4).
narrative_ontology:measurement_basis(equa_tr_t1850, observed).
narrative_ontology:measurement(equa_tr_t1865, equality_clause_scope__restrictive_originalist, theater_ratio, 1865, 0.42).
narrative_ontology:measurement_basis(equa_tr_t1865, observed).
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__restrictive_originalist, theater_ratio, 1868, 0.42).
narrative_ontology:measurement_basis(equa_tr_t1868, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1787, equality_clause_scope__restrictive_originalist, base_extractiveness, 1787, 0.71).
narrative_ontology:measurement_basis(equa_be_t1787, observed).
narrative_ontology:measurement(equa_be_t1808, equality_clause_scope__restrictive_originalist, base_extractiveness, 1808, 0.74).
narrative_ontology:measurement_basis(equa_be_t1808, observed).
narrative_ontology:measurement(equa_be_t1830, equality_clause_scope__restrictive_originalist, base_extractiveness, 1830, 0.76).
narrative_ontology:measurement_basis(equa_be_t1830, observed).
narrative_ontology:measurement(equa_be_t1850, equality_clause_scope__restrictive_originalist, base_extractiveness, 1850, 0.77).
narrative_ontology:measurement_basis(equa_be_t1850, observed).
narrative_ontology:measurement(equa_be_t1865, equality_clause_scope__restrictive_originalist, base_extractiveness, 1865, 0.78).
narrative_ontology:measurement_basis(equa_be_t1865, observed).
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__restrictive_originalist, base_extractiveness, 1868, 0.78).
narrative_ontology:measurement_basis(equa_be_t1868, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1787, equality_clause_scope__restrictive_originalist, suppression_requirement, 1787, 0.68).
narrative_ontology:measurement_basis(equa_su_t1787, observed).
narrative_ontology:measurement(equa_su_t1808, equality_clause_scope__restrictive_originalist, suppression_requirement, 1808, 0.71).
narrative_ontology:measurement_basis(equa_su_t1808, observed).
narrative_ontology:measurement(equa_su_t1830, equality_clause_scope__restrictive_originalist, suppression_requirement, 1830, 0.75).
narrative_ontology:measurement_basis(equa_su_t1830, observed).
narrative_ontology:measurement(equa_su_t1850, equality_clause_scope__restrictive_originalist, suppression_requirement, 1850, 0.79).
narrative_ontology:measurement_basis(equa_su_t1850, observed).
narrative_ontology:measurement(equa_su_t1865, equality_clause_scope__restrictive_originalist, suppression_requirement, 1865, 0.81).
narrative_ontology:measurement_basis(equa_su_t1865, observed).
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__restrictive_originalist, suppression_requirement, 1868, 0.81).
narrative_ontology:measurement_basis(equa_su_t1868, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1787, tn=1868
narrative_ontology:measurement(equa_grid_01, equality_clause_scope__restrictive_originalist, accessibility_collapse(class), 1787, 0.75).
narrative_ontology:measurement(equa_grid_02, equality_clause_scope__restrictive_originalist, accessibility_collapse(class), 1868, 0.8).
narrative_ontology:measurement(equa_grid_03, equality_clause_scope__restrictive_originalist, accessibility_collapse(individual), 1787, 0.65).
narrative_ontology:measurement(equa_grid_04, equality_clause_scope__restrictive_originalist, accessibility_collapse(individual), 1868, 0.72).
narrative_ontology:measurement(equa_grid_05, equality_clause_scope__restrictive_originalist, accessibility_collapse(organizational), 1787, 0.7).
narrative_ontology:measurement(equa_grid_06, equality_clause_scope__restrictive_originalist, accessibility_collapse(organizational), 1868, 0.76).
narrative_ontology:measurement(equa_grid_07, equality_clause_scope__restrictive_originalist, accessibility_collapse(structural), 1787, 0.78).
narrative_ontology:measurement(equa_grid_08, equality_clause_scope__restrictive_originalist, accessibility_collapse(structural), 1868, 0.82).
narrative_ontology:measurement(equa_grid_09, equality_clause_scope__restrictive_originalist, resistance(class), 1787, 0.25).
narrative_ontology:measurement(equa_grid_10, equality_clause_scope__restrictive_originalist, resistance(class), 1868, 0.72).
narrative_ontology:measurement(equa_grid_11, equality_clause_scope__restrictive_originalist, resistance(individual), 1787, 0.15).
narrative_ontology:measurement(equa_grid_12, equality_clause_scope__restrictive_originalist, resistance(individual), 1868, 0.55).
narrative_ontology:measurement(equa_grid_13, equality_clause_scope__restrictive_originalist, resistance(organizational), 1787, 0.22).
narrative_ontology:measurement(equa_grid_14, equality_clause_scope__restrictive_originalist, resistance(organizational), 1868, 0.68).
narrative_ontology:measurement(equa_grid_15, equality_clause_scope__restrictive_originalist, resistance(structural), 1787, 0.28).
narrative_ontology:measurement(equa_grid_16, equality_clause_scope__restrictive_originalist, resistance(structural), 1868, 0.75).
narrative_ontology:measurement(equa_grid_17, equality_clause_scope__restrictive_originalist, stakes_inflation(class), 1787, 0.6).
narrative_ontology:measurement(equa_grid_18, equality_clause_scope__restrictive_originalist, stakes_inflation(class), 1868, 0.75).
narrative_ontology:measurement(equa_grid_19, equality_clause_scope__restrictive_originalist, stakes_inflation(individual), 1787, 0.48).
narrative_ontology:measurement(equa_grid_20, equality_clause_scope__restrictive_originalist, stakes_inflation(individual), 1868, 0.62).
narrative_ontology:measurement(equa_grid_21, equality_clause_scope__restrictive_originalist, stakes_inflation(organizational), 1787, 0.55).
narrative_ontology:measurement(equa_grid_22, equality_clause_scope__restrictive_originalist, stakes_inflation(organizational), 1868, 0.7).
narrative_ontology:measurement(equa_grid_23, equality_clause_scope__restrictive_originalist, stakes_inflation(structural), 1787, 0.65).
narrative_ontology:measurement(equa_grid_24, equality_clause_scope__restrictive_originalist, stakes_inflation(structural), 1868, 0.78).
narrative_ontology:measurement(equa_grid_25, equality_clause_scope__restrictive_originalist, suppression(class), 1787, 0.75).
narrative_ontology:measurement(equa_grid_26, equality_clause_scope__restrictive_originalist, suppression(class), 1868, 0.83).
narrative_ontology:measurement(equa_grid_27, equality_clause_scope__restrictive_originalist, suppression(individual), 1787, 0.72).
narrative_ontology:measurement(equa_grid_28, equality_clause_scope__restrictive_originalist, suppression(individual), 1868, 0.8).
narrative_ontology:measurement(equa_grid_29, equality_clause_scope__restrictive_originalist, suppression(organizational), 1787, 0.68).
narrative_ontology:measurement(equa_grid_30, equality_clause_scope__restrictive_originalist, suppression(organizational), 1868, 0.78).
narrative_ontology:measurement(equa_grid_31, equality_clause_scope__restrictive_originalist, suppression(structural), 1787, 0.8).
narrative_ontology:measurement(equa_grid_32, equality_clause_scope__restrictive_originalist, suppression(structural), 1868, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__restrictive_originalist, 0.15).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, fourteenth_amendment_reconstruction_scope).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, franchise_exclusion_gender_based).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, property_requirement_suffrage).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the equality_clause_scope kernel. The sibling readings (expansive_universalist and progressive_textualist) are separate constraint stories with different ε values, beneficiary/victim sets, and classification outcomes. The three stories form a constraint family linked by network.affects_constraints: the restrictive originalist reading influences both siblings by defining a legal baseline they must either accept or overcome. Decomposition follows the ε-invariance principle (DP-001): measuring 'equality clause scope' through the restrictive originalist lens yields a high-extraction, suppressive reading; measuring through the expansive universalist lens yields a different ε (lower extraction from excluded groups, higher coordination function); measuring through the progressive textualist lens yields intermediate extraction with explicit amendment gates. Each reading is ε-invariant within itself; the readings differ in what they measure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, powerless, 0.95).
constraint_indexing:directionality_override(equality_clause_scope__restrictive_originalist, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
