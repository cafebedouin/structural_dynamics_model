% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__judicial_supremacy_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: basic_law_interpretive_boundary__judicial_supremacy_reading
 *   human_readable: Basic Law Judicial Supremacy Reading
 *   domain: constitutional_law/comparative_constitutionalism/judicial_review_theory
 *
 * SUMMARY:
 *   This constraint instantiates the judicial_supremacy_reading of the
 *   contested kernel basic_law_interpretive_boundary in Israeli
 *   constitutional law. Under this reading, the Basic Laws constitute a
 *   higher-order normative framework that the Supreme Court authoritatively
 *   interprets, and the Court's invalidation of contradictory Knesset
 *   legislation is binding on the legislature. The constraint coordinates
 *   constitutional rights protection and legal stability while simultaneously
 *   extracting legislative sovereignty from the elected Knesset and
 *   transferring it to the judicial branch. The claim/metric independence is
 *   maintained: the judicial supremacy reading presents itself as
 *   constitutional necessity (mountain- or rope-like), while the authored
 *   metrics capture the substantial extraction, active enforcement, and
 *   suppressed alternatives that characterize its actual operation.
 *
 * KEY AGENTS:
 *   - Supreme Court: agenda_setter (institutional/constrained/national) â interprets Basic Laws, invalidates legislation, accumulates constitutional authority
 *   - Rights claimants: beneficiary (organized/mobile/national) â gain litigation-based veto over legislation through constitutional petitions
 *   - Constitutional bar: beneficiary (organized/mobile/national) â professional community whose standing depends on robust judicial review
 *   - Knesset majority: payer (institutional/constrained/national) â elected majority whose statutes can be nullified by judicial review
 *   - Electorate: payer (moderate/constrained/national) â citizens whose electoral preferences are subject to judicial override
 *   - Comparative constitutional scholars: observer (analytical/analytical/global) â analytical seat tracking the experiment in uncodified constitutional supremacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.8).
domain_priors:suppression_score(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.75).
domain_priors:theater_ratio(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__judicial_supremacy_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__judicial_supremacy_reading, "Basic Law Judicial Supremacy Reading").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__judicial_supremacy_reading, "constitutional_law/comparative_constitutionalism/judicial_review_theory").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__judicial_supremacy_reading, '7d14f290-f168-4438-bbc2-78aa8041a7c6').
narrative_ontology:cs_kernel_codification('7d14f290-f168-4438-bbc2-78aa8041a7c6', formalized).
narrative_ontology:cs_authority_grounding('7d14f290-f168-4438-bbc2-78aa8041a7c6', lineage).
narrative_ontology:cs_interpretation_layer_present('7d14f290-f168-4438-bbc2-78aa8041a7c6').
narrative_ontology:cs_reading_relation('7d14f290-f168-4438-bbc2-78aa8041a7c6', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('7d14f290-f168-4438-bbc2-78aa8041a7c6', basic_law_interpretive_boundary__balanced_contestation_reading, coexists_with).
narrative_ontology:cs_axiom('7d14f290-f168-4438-bbc2-78aa8041a7c6', foundational, judicial_final_interpretive_authority).
narrative_ontology:cs_axiom_status(judicial_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('7d14f290-f168-4438-bbc2-78aa8041a7c6', judicial_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('7d14f290-f168-4438-bbc2-78aa8041a7c6', foundational, basic_law_binding_hierarchy).
narrative_ontology:cs_axiom_status(basic_law_binding_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('7d14f290-f168-4438-bbc2-78aa8041a7c6', basic_law_binding_hierarchy, conventional).
narrative_ontology:cs_reference_frame('7d14f290-f168-4438-bbc2-78aa8041a7c6', basic_law_supremacy_framework).
narrative_ontology:cs_drift_state('7d14f290-f168-4438-bbc2-78aa8041a7c6', judicial_reform_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7d14f290-f168-4438-bbc2-78aa8041a7c6', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__judicial_supremacy_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_bar).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__judicial_supremacy_reading, electorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Basic Laws as a higher-order constitutional framework and enforces this interpretation by invalidating Knesset legislation it deems contradictory. Accumulates institutional authority and legitimacy through the exercise of judicial review, but is structurally constrained by legal-professional norms, precedent, and the threat of political backlash or institutional override.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Utilize constitutional petitions to challenge legislation before the Supreme Court, gaining a de facto veto mechanism over laws they claim violate Basic Law protections. Their access to rights-protection depends on the Court's willingness to entertain petitions and issue invalidating judgments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, rights_claimants, beneficiary,
    organized, biographical, mobile, national).

% Legal practitioners specializing in constitutional litigation before the Supreme Court. Their professional standing, fees, influence, and career trajectories depend on the existence of a robust judicial review doctrine that channels constitutional disputes into the courtroom rather than the political arena.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, constitutional_bar, beneficiary,
    organized, biographical, mobile, national).

% Elected legislative majority whose statutes can be nullified by the Supreme Court on constitutional grounds. Attempts to override judicial review through Basic Law amendments or ordinary legislation face institutional, legal, and public obstacles, and risk triggering prolonged constitutional crises.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, knesset_majority, payer,
    institutional, biographical, constrained, national).

% Citizens whose electoral preferences are translated into legislation that may subsequently be invalidated by judicial review. Their ability to override constitutional interpretation through electoral politics is structurally limited by the Court's claim to final authority over the meaning and hierarchy of the Basic Laws.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, electorate, payer,
    moderate, generational, constrained, national).

% Academic observers who analyze Israel's constitutional experiment within comparative frameworks, noting the unusual combination of uncodified constitutional supremacy, strong-form judicial review, and the absence of a formal constitutional ratification process.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a hierarchically stable legal framework where fundamental rights and constitutional principles are insulated from transient parliamentary majorities and coalition instability, enabling predictable judicial interpretation and rights enforcement.
% TRANSFER_FUNCTION: Transfers final interpretive authority over the normative status of legislation from the elected Knesset to the judicial branch, and grants rights-claimants a litigation-based veto mechanism over laws they claim violate Basic Law protections.
% ABSENT_VOICES: Parliamentary sovereignty advocates who view the Knesset as the sole legitimate sovereign; ultra-orthodox and nationalist political factions whose agendas are routinely invalidated by the Court; occupied Palestinian populations subject to military law with highly limited access to constitutional protections; elected officials from peripheral constituencies who experience the Court as an elite, secular check.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, the Knesset would regain unrestricted legislative capacity, Basic Law interpretation would become a political rather than juridical function, the rights-petition system would lose its nullification power, and Israel's constitutional structure would shift toward parliamentary sovereignty or majoritarian democracy.
% FOUNDING_PROBLEM: The absence of a single formal constitution creating legal instability and the vulnerability of fundamental rights, national identity provisions, and basic state structures to unstable coalition politics and simple parliamentary majorities in a deeply polarized polity.
% FOUNDING_PROBLEM_CORROBORATION: The Supreme Court and constitutional scholars attest the problem remains acute, citing ongoing threats to minority rights and rule of law. The Knesset majority and parliamentary sovereignty advocates attest the problem was substantially resolved by the Basic Laws themselves and that judicial supremacy creates a new democratic crisis. Comparative constitutionalists note that other Westminster-influenced systems operate without judicial supremacy, suggesting the founding problem does not uniquely mandate this arrangement.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__judicial_supremacy_reading, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because the constraint transfers final legislative authority from an elected parliament to an unelected court, generating a persistent democratic deficit. Suppression is high (0.75) because parliamentary sovereignty alternatives are legally suppressed through the doctrine of binding judicial review and politically delegitimized as majoritarian threats. Accessibility collapse is very high (0.85): within Israeli legal culture, the supremacy of Basic Laws and judicial interpretive authority are treated as professionally axiomatic, making alternatives nearly unthinkable for trained lawyers. Resistance is high (0.78) because political actors have repeatedly contested the doctrine, most acutely during the 2023-2024 judicial reform crisis. Theater ratio is moderate (0.45): genuine rights protection occurs, but an increasing share of judicial activity performs constitutional guardianship under political siege, substituting institutional self-defense for pure legal reasoning. The temporal series trace the doctrine's evolution from the 1992 Basic Laws through the progressive assertion of strong-form review and the recent political backlash.
 *
 * PERSPECTIVAL GAP:
 *   The Supreme Court seat computes the constraint as coordination (maintaining constitutional order and protecting minority rights against transient majorities), while the Knesset majority and electorate seats compute it as extraction (democratic sovereignty nullified by judicial veto). Rights claimants experience a rope-like protective structure, whereas the parliamentary sovereignty faction experiences a snare-like barrier to self-governance. The engine derives this divergence from the structural data: low directionality for beneficiaries (Court, rights claimants, bar) and high directionality for targets (Knesset, electorate) with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court sits near the beneficiary pole: it is the seat to which legislative sovereignty is transferred and which collects institutional authority from the constraint's operation. Rights claimants and the constitutional bar are direct beneficiaries of the litigation channel the constraint creates. The Knesset majority and the electorate sit near the target pole: they bear the cost of legislative nullification and have no available exit that does not trigger a constitutional crisis. The comparative scholars seat is analytical and does not feed directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mislabeling this constraint as a Mountain (constitutional law as inevitable natural law) or as a pure Rope (coordination without extraction). The genuine coordination functionârights protection and legal stabilityâis acknowledged, but the asymmetric extraction of democratic sovereignty is structurally required: beneficiaries (Court, claimants) are identified alongside victims (Knesset, electorate), and active enforcement is required to maintain the arrangement. This prevents the common cover story that judicial review is purely protective rather than also power-concentrating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drafters_intent_supremacy,
    'Did the drafters of the Basic Laws intend to authorize judicial supremacy and binding judicial review over Knesset legislation, or merely to entrench specific statutory provisions without creating a comprehensive constitutional hierarchy?',
    'Historical analysis of Knesset plenum and committee debate records, private drafter memoranda, and contemporaneous legal scholarship from the enactment periods of the Basic Laws (1958-1992).',
    'If drafters did not intend supremacy, the constraint''s legitimacy shifts from enacted constitutional will to judicial construction, increasing extraction and potentially reclassifying toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drafters_intent_supremacy, empirical, 'Original intent of Basic Law drafters regarding judicial review authority').

omega_variable(
    democratic_deficit_legitimacy,
    'Does the counter-majoritarian difficulty generate a persistent democratic legitimacy deficit that undermines the constraint''s coordination function, or is the deficit a sustainable price of rights protection?',
    'Comparative analysis of democratic stability and public compliance in systems with strong-form judicial review versus parliamentary sovereignty; longitudinal public opinion data on Court legitimacy in Israel.',
    'A severe, unacknowledged legitimacy deficit would indicate the coordination story is largely cover for power transfer, pushing classification toward snare; a manageable deficit supports tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_deficit_legitimacy, conceptual, 'Democratic legitimacy cost of judicial supremacy').

omega_variable(
    enforcement_capacity_hostile_legislature,
    'Can the Supreme Court maintain effective enforcement of its nullification power against a Knesset that refuses legislative compliance and actively deploys court-curbing measures or override legislation?',
    'Observation of institutional conflict episodes, compliance rates with controversial rulings, and the political fate of judicial reform legislative packages.',
    'If enforcement fails despite continued doctrinal assertions, the constraint decays toward piton (theatrical maintenance without effective extraction); if enforcement succeeds only through escalating crisis, suppression rises and the constraint may tighten toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_hostile_legislature, empirical, 'Enforcement durability under political attack').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__judicial_supremacy_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t8, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t16, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(basic_law_judicial_supremacy_tr_t32, basic_law_interpretive_boundary__judicial_supremacy_reading, theater_ratio, 32, 0.45).

% Extraction over time
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t8, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t16, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(basic_law_judicial_supremacy_be_t32, basic_law_interpretive_boundary__judicial_supremacy_reading, base_extractiveness, 32, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t0, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t8, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t16, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t24, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(basic_law_judicial_supremacy_su_t32, basic_law_interpretive_boundary__judicial_supremacy_reading, suppression_requirement, 32, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__judicial_supremacy_reading, balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel basic_law_interpretive_boundary. The judicial_supremacy_reading claims the Supreme Court holds final interpretive authority over Basic Laws with binding effect on the Knesset. The parliamentary_sovereignty_reading locates ultimate authority in the Knesset to interpret and amend Basic Laws. The balanced_contestation_reading distributes bounded authority between both institutions. Each reading instantiates a structurally distinct constraint with different epsilon values, beneficiary/victim structures, and directionalities; they form a constraint family linked through mutual contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
