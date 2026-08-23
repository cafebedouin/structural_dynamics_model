% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__aspirational_sovereignty_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR as Moral Guidance Requiring State Consent for Binding Obligation
 *   domain: international_law/political_philosophy/human_rights_doctrine
 *
 * SUMMARY:
 *   The aspirational sovereignty reading of the UDHR holds that the
 *   Declaration provides moral guidance only, and that binding international
 *   obligations require explicit state consent through treaty ratification.
 *   This reading treats state sovereignty as a structural mountain — a
 *   natural law of the international system that cannot be overridden without
 *   the state's agreement. The UDHR's preamble ('as a common standard of
 *   achievement for all peoples and all nations') and the contemporaneous
 *   decision to adopt a non-binding declaration rather than a binding
 *   covenant are cited as evidence. Under this reading, states retain a veto
 *   over legal obligation; international tribunals lack coercive power absent
 *   ratification; and the constraint extracts minimally from state autonomy.
 *   The reading coexists with the binding universalism reading (which claims
 *   the UDHR established justiciable rights regardless of consent) and the
 *   customary emergence reading (which claims UDHR provisions have become
 *   binding through state practice and opinio juris). The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as mountain (sovereignty as natural
 *   law) while the authored metrics describe low but non-zero extraction and
 *   suppression — the engine measures that divergence; do not reconcile the
 *   claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__aspirational_sovereignty_reading, 0.15).
domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, 0.1).
domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__aspirational_sovereignty_reading, mountain).
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR as Moral Guidance Requiring State Consent for Binding Obligation").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international_law/political_philosophy/human_rights_doctrine").

domain_priors:emerges_naturally(udhr_authority__aspirational_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, 'eee2e368-85e1-410d-8adf-ec2d0bf44ec5').
narrative_ontology:cs_kernel_codification('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', fixed_text).
narrative_ontology:cs_authority_grounding('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', lineage).
narrative_ontology:cs_interpretation_layer_present('eee2e368-85e1-410d-8adf-ec2d0bf44ec5').
narrative_ontology:cs_reading_relation('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', udhr_authority__binding_universalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', foundational, state_consent_required_for_obligation).
narrative_ontology:cs_axiom_status(state_consent_required_for_obligation, holdable).
narrative_ontology:cs_axiom_grounding('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', state_consent_required_for_obligation, conventional).
narrative_ontology:cs_axiom('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', foundational, udhr_as_moral_not_legal_instrument).
narrative_ontology:cs_axiom_status(udhr_as_moral_not_legal_instrument, holdable).
narrative_ontology:cs_axiom_grounding('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', udhr_as_moral_not_legal_instrument, conventional).
narrative_ontology:cs_reference_frame('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eee2e368-85e1-410d-8adf-ec2d0bf44ec5', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, individuals_in_rights_violating_states).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, marginalized_populations_without_state_protection).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, sovereign_equality_principle).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, consent_based_obligation_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, westphalian_non_intervention_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the consent gateway to binding obligation. They drafted the UDHR as a declaration precisely to avoid legal obligation while gaining moral authority. They ratify treaties selectively, enter reservations, and invoke sovereignty to reject external adjudication. They benefit from the moral prestige of endorsing the UDHR without the legal costs of compliance.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the human cost when their state withholds consent to binding obligations. The UDHR's moral language gives them a vocabulary for claim-making, but the consent requirement blocks legal remedy. They cannot exit the state's jurisdiction; they are trapped in the gap between moral aspiration and legal reality.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, individuals_in_rights_violating_states, payer,
    powerless, biographical, trapped, national).

% Populations (stateless persons, minorities, indigenous groups) whose rights are violated by state action or inaction, and who lack even the theoretical protection of a state willing to consent on their behalf. They are doubly excluded: from the consent mechanism (no state represents them) and from the moral discourse (their specific vulnerabilities are not centered in the UDHR's universalist framing).
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, marginalized_populations_without_state_protection, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(udhr_authority__aspirational_sovereignty_reading, marginalized_populations_without_state_protection, excluded).

% Courts and treaty bodies (ICJ, ICC, regional human rights courts, UN treaty bodies) that cite the UDHR as interpretive authority but lack independent coercive power. Their legitimacy depends on state acceptance; they navigate between the aspirational reading (deferring to state consent) and the universalist reading (asserting inherent justiciability).
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, international_tribunals, observer,
    institutional, generational, analytical, global).

% Actors who use the UDHR's moral authority to pressure states, document violations, and push for treaty ratification. They are excluded from the state consent mechanism (no formal vote) but operate in the moral and political space the UDHR creates. Their effectiveness varies with the reading's dominance: when the aspirational reading prevails, their leverage is reputational only; when universalist or customary readings gain ground, their legal tools expand.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, human_rights_advocates_and_ngos, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared moral vocabulary and aspirational standard across the international system without requiring states to surrender sovereignty. Provides a common reference point for diplomatic discourse, naming and shaming, and the gradual development of binding treaty law — all gated by state consent.
% TRANSFER_FUNCTION: Moves the power to create binding legal obligation from automatic universal entitlement to state consent. The transfer is: obligation-creation authority → sovereign states. The cost (non-enforcement of rights) is borne by individuals in non-consenting states. The benefit (sovereignty preservation) accrues to states.
% ABSENT_VOICES: Individuals in rights-violating states who would object to the consent gate if they had access to the forums where sovereignty is negotiated. Stateless persons and populations without effective state representation. Future generations who inherit the international system's structural choices. These voices are absent because the state-centric international system has no formal mechanism for their participation in the consent calculus.
% DISAPPEARANCE_RATIONALE: If the aspirational sovereignty reading vanished overnight — i.e., if the UDHR were universally accepted as creating binding obligations without further consent — the international human rights system would fundamentally reorganize: treaty ratification would become declaratory rather than constitutive, customary law would accelerate, state reservations would lose legal effect, and the sovereignty veto would collapse. The world of state consent would rearrange into a world of automatic obligation.
% FOUNDING_PROBLEM: Post-WWII need for a universal moral framework that condemned atrocity without requiring states to surrender sovereign discretion over domestic jurisdiction. The UDHR was deliberately crafted as a non-binding declaration because the major powers (especially the US and USSR) would not accept a binding covenant that exposed their own practices to international adjudication.
% FOUNDING_PROBLEM_CORROBORATION: The drafting history (travaux préparatoires) corroborates the sovereignty-protective intent: the Third Committee debates show consistent resistance to language implying legal obligation. However, the UDHR's own preamble ('every individual and every organ of society shall strive... by progressive measures, national and international, to secure their universal and effective recognition') and the subsequent proliferation of binding treaties (ICCPR, ICESCR, CERD, CEDAW, CAT, CRC, CRPD) corroborate the rival view that the founding problem was a transitional compromise, not a permanent structural settlement. No single authority outside the benefiting states definitively resolves this.
narrative_ontology:disappearance_verdict(udhr_authority__aspirational_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__aspirational_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__aspirational_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_authority__aspirational_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__aspirational_sovereignty_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__aspirational_sovereignty_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, ExtMetricName, E),
    domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(udhr_authority__aspirational_sovereignty_reading),
    narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(udhr_authority__aspirational_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the reading's own structural logic posits that states are not compelled — they consent to be bound. The 0.15 residual reflects the moral and reputational pressure the UDHR exerts even on non-consenting states (naming and shaming, diplomatic costs). Suppression is low (0.1) because the constraint does not actively prevent alternatives — states can and do reject binding obligations, and the reading celebrates this. Theater ratio is low (0.1) because the moral guidance function is genuine, not performative; the UDHR genuinely coordinates a shared moral vocabulary. Accessibility collapse is moderately high (0.75) because once the sovereignty principle is accepted, alternatives (automatic universal obligation) appear structurally impossible — but not completely, as the sibling readings demonstrate. Resistance is low (0.15) because the reading aligns with the interests of the most powerful actors (sovereign states).
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign state seat, the arrangement is a genuine mountain — sovereignty protects the state from external coercion and the UDHR's moral guidance is a voluntary commitment. From the individual rights-holder seat in a non-consenting state, the same structure operates as a snare — the moral language provides cover while the consent requirement blocks remedy. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states are the primary beneficiaries (d near beneficiary end) — they retain veto power over legal obligation and control the consent gateway. Individuals in rights-violating states and marginalized populations are the payers (d near target end) — they bear the cost of non-enforcement when their state withholds consent. International tribunals sit near symmetric (d ~0.5) — they gain moral authority from the UDHR but lack coercive power without state consent. Human rights advocates are excluded from the consent mechanism but not from the moral discourse — their exclusion is structural (no formal role in state consent) not absolute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII moral framework without surrendering sovereignty) is contested: states and realists argue sovereignty protection remains essential to international order; human rights advocates argue the founding problem has been superseded by the evolution of binding treaty law and customary norms. The constraint persists not because the founding problem is live, but because the sovereignty veto is a structural feature of the state system — a mountain that may be false summit if it serves to extract impunity for rights violations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine mountain of sovereignty (natural law of state consent) or a constructed constraint that benefits sovereign states by shielding them from binding human rights obligations?',
    'Compare the drafting history of the UDHR against subsequent state practice: if states consistently treated the UDHR as non-binding by design and resisted legalization, the mountain claim holds; if states instrumentalized ''aspirational'' language to avoid obligations while benefiting from the UDHR''s moral authority, the false summit hypothesis gains weight.',
    'If false summit, the constraint reclassifies to tangled_rope (coordination of state consent + asymmetric extraction from rights-holders). If genuine mountain, the aspirational reading correctly describes a structural feature of the international system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural law vs. constructed sovereignty veto').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the lack of binding enforcement structural (no world government exists to coerce states) or internalized (states and elites have absorbed the belief that sovereignty legitimately trumps individual rights)?',
    'Post-exit suppression trajectory: examine states that have accepted binding human rights treaties (ICCPR, regional systems). If suppression persists domestically despite treaty ratification, internalized component is significant. If suppression lifts with treaty acceptance, structural component dominates.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — states carry the non-enforcement norm with them even into treaty regimes. If purely structural, the reading''s low suppression score is descriptively accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized non-enforcement mechanism').

omega_variable(
    customary_law_formation_boundary,
    'At what point does widespread state practice and opinio juris transform the UDHR''s aspirational provisions into binding customary law, overriding the state consent requirement this reading posits?',
    'Track ICJ and regional court jurisprudence citing UDHR provisions as customary law; measure state acceptance of treaty bodies'' interpretive authority; assess whether persistent objection doctrine still operates for core UDHR rights.',
    'If customary emergence has overtaken the consent gate for core provisions, the aspirational reading describes a historical stage, not the current constraint. The reading''s claimed mountain would be a former mountain (piton) or a mountain with eroded scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_formation_boundary, empirical, 'Threshold where aspiration becomes binding custom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_aspirational_sovereignty_tr_t1948, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(udhr_aspirational_sovereignty_tr_t1966, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1966, 0.07).
narrative_ontology:measurement(udhr_aspirational_sovereignty_tr_t1976, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1976, 0.08).
narrative_ontology:measurement(udhr_aspirational_sovereignty_tr_t1990, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(udhr_aspirational_sovereignty_tr_t2000, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(udhr_aspirational_sovereignty_tr_t2010, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(udhr_aspirational_sovereignty_tr_t2020, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(udhr_aspirational_sovereignty_be_t1948, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1948, 0.05).
narrative_ontology:measurement(udhr_aspirational_sovereignty_be_t1966, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1966, 0.08).
narrative_ontology:measurement(udhr_aspirational_sovereignty_be_t1976, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1976, 0.1).
narrative_ontology:measurement(udhr_aspirational_sovereignty_be_t1990, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement(udhr_aspirational_sovereignty_be_t2000, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2000, 0.14).
narrative_ontology:measurement(udhr_aspirational_sovereignty_be_t2010, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(udhr_aspirational_sovereignty_be_t2020, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2020, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(udhr_aspirational_sovereignty_su_t1948, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1948, 0.05).
narrative_ontology:measurement(udhr_aspirational_sovereignty_su_t1966, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1966, 0.07).
narrative_ontology:measurement(udhr_aspirational_sovereignty_su_t1976, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1976, 0.08).
narrative_ontology:measurement(udhr_aspirational_sovereignty_su_t1990, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 1990, 0.09).
narrative_ontology:measurement(udhr_aspirational_sovereignty_su_t2000, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2000, 0.1).
narrative_ontology:measurement(udhr_aspirational_sovereignty_su_t2010, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(udhr_aspirational_sovereignty_su_t2020, udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 2020, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__aspirational_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(udhr_authority__aspirational_sovereignty_reading, 0.08).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__customary_emergence_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, iccpr_consent_gate).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, regional_human_rights_systems).

% DUAL FORMULATION NOTE:
% The udhr_authority kernel decomposes into three constraint stories: this aspirational sovereignty reading (mountain claim, low ε, state beneficiaries), the binding universalism reading (snare/tangled rope claim, higher ε, individual beneficiaries), and the customary emergence reading (tangled rope claim, moderate ε, mixed beneficiaries). The aspirational reading's insistence on state consent structurally influences the customary emergence reading by raising the threshold for customary law formation. The binding universalism reading coexists as a rival normative position held by different institutional actors (treaty bodies, regional courts, NGOs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__aspirational_sovereignty_reading, institutional, 0.15).
constraint_indexing:directionality_override(udhr_authority__aspirational_sovereignty_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
