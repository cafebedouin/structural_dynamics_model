% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Practical Survival-Knowledge Transmission (Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Across catastrophe-adapted communities, ritual calendars and sequences
 *   carry embedded practical knowledge — when to plant, when to store, how to
 *   ration, when to move — inside a form that outwardly reads as religious or
 *   symbolic observance. Under this reading, the ritual's survival value IS
 *   this embedded technical content. Transmission of the full sequence is
 *   controlled by specialist lineages, and where it flows successfully
 *   (notably to diaspora communities that redeploy it under new conditions)
 *   it functions as genuine adaptive infrastructure. Where transmission has
 *   degraded — through death of specialists, migration, secrecy, or simple
 *   entropy — communities are left performing a hollowed shell that no longer
 *   carries the competence it once encoded, while believing (or hoping) it
 *   still does.
 *
 * KEY AGENTS:
 *   - diaspora_adaptive_households: primary beneficiary (moderate/mobile) — successfully redeploys embedded knowledge in new contexts
 *   - ritual_specialist_lineages: agenda-setter (organized/constrained) — controls depth and completeness of transmission
 *   - form_preserving_home_communities: primary victim (powerless/trapped) — bears the cost of a hollowed system
 *   - younger_generation_practitioners: secondary victim (powerless/constrained) — inherits obligation without competence
 *   - external_aid_and_development_agencies: excluded — technical substitute knowledge not integrated
 *   - ethnographic_and_folklore_researchers: analytical observer — makes the degradation legible
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Practical Survival-Knowledge Transmission (Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '9c5cecd1-ca20-43ff-bcbc-1928f7e93398').
narrative_ontology:cs_kernel_codification('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', distributed).
narrative_ontology:cs_authority_grounding('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', practice).
narrative_ontology:cs_interpretation_layer_present('9c5cecd1-ca20-43ff-bcbc-1928f7e93398').
narrative_ontology:cs_reading_relation('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', foundational, ritual_content_is_technical_payload).
narrative_ontology:cs_axiom_status(ritual_content_is_technical_payload, holdable).
narrative_ontology:cs_axiom_grounding('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', ritual_content_is_technical_payload, empirically_contingent).
narrative_ontology:cs_axiom('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', secondary, form_without_content_is_functional_failure).
narrative_ontology:cs_axiom_status(form_without_content_is_functional_failure, holdable).
narrative_ontology:cs_axiom_grounding('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', form_without_content_is_functional_failure, instrumental).
narrative_ontology:cs_reference_frame('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', intact_specialist_transmission_chain).
narrative_ontology:cs_drift_state('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', post_displacement_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9c5cecd1-ca20-43ff-bcbc-1928f7e93398', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_adaptive_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, ritual_specialist_lineages).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, form_preserving_home_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, younger_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Left the ancestral territory under catastrophe pressure (famine, flood, displacement) and carried the ritual calendar with them. Where the embedded timing and resource-management content of the ritual is still legible, they successfully repurpose it to new terrain — planting cycles, water rationing schedules, kin-obligation networks in unfamiliar cities. The ritual functions as a portable technical manual disguised as observance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_adaptive_households, beneficiary,
    moderate, generational, mobile, continental).

% Hereditary or apprenticed keepers of the full ritual sequence, including the parts that read as merely symbolic to outsiders but actually encode drought signs, food-storage timing, or evacuation triggers. They control who receives the full transmission versus the truncated public performance, and their authority rests on being the ones who can still 'read' the embedded knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_specialist_lineages, agenda_setter,
    organized, generational, constrained, regional).

% Continue performing the ritual's outward form at the original site, but the practical content (which crops, which timing, which resource contingencies) no longer matches present conditions because the specialists who understood the embedded knowledge died, migrated, or withheld it. They pay the cost of a catastrophe-response system that looks intact but has been hollowed of the competence it was built to carry.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, form_preserving_home_communities, payer,
    powerless, biographical, trapped, local).

% Learn the ritual sequence by rote from elders who themselves received only the partial or symbolic layer, not the full technical content. They inherit the obligation to perform without the survival competence the obligation was originally meant to transmit, and have no way of knowing what was lost because the loss is invisible inside a form that still looks complete.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, younger_generation_practitioners, payer,
    powerless, biographical, constrained, local).

% Arrive after acute crises with technical resource-management expertise that could substitute for or supplement degraded ritual knowledge, but are rarely consulted by or granted access to the specialist lineages, and rarely recognize ritual practice as a competing technical knowledge system worth interfacing with.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, external_aid_and_development_agencies, excluded,
    institutional, immediate, analytical, global).

% Document ritual sequences across sites and generations, sometimes reconstructing the practical content embedded in symbolic form from comparative and archival evidence, without power to restore transmission but with the ability to make the loss visible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ethnographic_and_folklore_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual compresses and transmits time-sensitive, high-stakes practical knowledge (when to plant, store, move, ration, or shelter) across generations without requiring literacy or centralized record-keeping — a genuine solution to the coordination problem of preserving survival competence under conditions where formal institutions may not persist.
% TRANSFER_FUNCTION: Moves technical competence (timing knowledge, resource-management heuristics, adaptation strategies) from specialist lineages and mobile diaspora households who retain or successfully redeploy it, at the cost of home communities and younger practitioners who inherit only the performative shell and bear the consequences when the shell fails to produce the material outcomes it once did.
% ABSENT_VOICES: External aid agencies with substitutable technical knowledge are not consulted by specialist lineages guarding transmission authority; they would argue for open technical exchange but have no standing inside the ritual's transmission hierarchy.
% DISAPPEARANCE_RATIONALE: Specialist lineages and diaspora households would say the world rearranges catastrophically — the loss of embedded timing and resource knowledge would remove a functioning early-warning and adaptation system. Form-preserving home communities, for whom the embedded content is already degraded, might see comparatively little material change, since they are already operating on the hollowed shell; the disagreement itself is evidence of where the competence has and hasn't survived.
% FOUNDING_PROBLEM: Communities facing recurring catastrophe (drought, flood, famine, displacement) needed a transmission mechanism for time-critical survival knowledge that would outlast individual memory, literacy access, and institutional continuity — knowledge that had to be actionable under duress, not merely inspirational.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic and folklore researchers, working from outside both the specialist lineages and the beneficiary diaspora households, attest via comparative reconstruction that some ritual sequences retain reconstructable technical content while others have measurably lost it; this corroboration comes from an analytical seat with no stake in either preserving or exposing the loss, though the specialist lineages themselves dispute how much of the researchers' reconstruction is accurate.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.48) is moderate rather than severe: this is not a predatory structure but a coordination mechanism that has partially decoupled from its function for some populations while continuing to work for others — the asymmetry, not raw severity, is the extractive signal. Theater ratio rises over the interval (0.20 to 0.55) because as embedded technical content erodes among form-preserving communities, the proportion of ritual activity that is purely performative (versus load-bearing) increases — this is the Goodhart-style drift the framework is built to detect: the form persists while the function it once indexed hollows out. Suppression (0.42) is moderate: specialist lineages restrict full transmission through apprenticeship gatekeeping and selective teaching, but this is soft suppression (social/institutional) rather than coercive enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora households and specialist lineages sit toward the beneficiary end: they retain or successfully redeploy the practical content, and the lineages additionally derive status/authority from being its custodians. Form-preserving home communities and younger practitioners sit toward the target end: they bear the ritual's continuing obligations and reputational stakes without receiving the competence that once justified those costs, and their exit options are constrained by the same social embeddedness that makes the ritual meaningful to them. Their trapped/constrained exit options amplify the effective extraction the engine will compute relative to the mobile diaspora seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetric errors: (1) treating the WHOLE ritual complex as pure extraction because SOME communities experience hollowed competence — this would erase the genuine, still-functioning coordination value the diaspora seat demonstrates; and (2) treating the whole complex as pure coordination because it once worked everywhere — this would erase the real, unevenly distributed cost borne by communities left holding an emptied form. Tangled Rope names both: a live coordination function coexisting with asymmetric extraction running through the same structure, requiring the active gatekeeping (enforcement) that keeps transmission uneven rather than universal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_competence_vs_symbol,
    'Is ritual''s survival-relevant content genuinely the embedded practical/technical knowledge (this reading), or is it the symbolic continuity of practice regardless of practical content (sibling symbol_survival_reading), or are both registers jointly necessary (sibling hybrid_encoding_reading)?',
    'Comparative ethnographic study of communities where ritual form persists but documented practical outcomes diverge (e.g., crop timing accuracy, resource shortfall rates) from communities where both form and content persist — if outcome divergence tracks content loss rather than form disruption, it favors the competence reading; if outcomes are unaffected by content loss so long as form continues, it favors the symbol reading.',
    'Adopting the symbol_survival_reading would dissolve the victim/beneficiary structure authored here entirely — form-preserving communities would be recast as the reading''s SUCCESS case rather than its casualty, and the tangled_rope classification would likely collapse toward rope or mountain. Adopting hybrid_encoding would preserve a coordination/extraction tension but redistribute which communities count as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_competence_vs_symbol, conceptual, 'Which kernel reading (competence, symbol, or hybrid) is the structurally correct account of ritual''s survival function — this story commits to competence_transmission_reading and authors its own independent epsilon accordingly.').

omega_variable(
    specialist_gatekeeping_intent,
    'Is the specialist lineages'' selective transmission a deliberate extraction strategy (withholding competence to preserve their own authority/status) or an unintended byproduct of apprenticeship attrition, migration, and mortality?',
    'Interviews with specialist lineage members across multiple sites, cross-referenced with succession records: deliberate withholding would show patterns of selective teaching correlated with status preservation; attrition would show patterns correlated with demographic and migration shocks uncorrelated with status motive.',
    'If deliberate, requires_active_enforcement is more clearly warranted and the tangled_rope classification is robust. If attritional, the constraint drifts toward piton — a degraded function persisting through inertia rather than active extraction, with no party meaningfully benefiting from the degradation itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(specialist_gatekeeping_intent, empirical, 'Whether transmission gatekeeping is intentional extraction or unintentional institutional decay.').

omega_variable(
    reconstructability_of_lost_content,
    'Once embedded practical content is lost from a home community''s transmission chain, can it be reconstructed from comparative ethnography, archival records, or diaspora-community cross-reference, or is the loss effectively permanent?',
    'Ethnographic reconstruction projects comparing surviving diaspora practice with degraded home-community practice, testing whether reconstructed content produces improved material outcomes when reintroduced.',
    'If reconstructable, the fixing_cost classification of ''prohibitive'' should be revisited — a bridging mechanism (researcher-mediated reconstruction) could lower the cost of restoring function. If not reconstructable, the loss is closer to irreversible and the prohibitive classification stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reconstructability_of_lost_content, empirical, 'Whether lost practical content is recoverable through external reconstruction efforts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 50, 0.51).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 60, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.37).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.39).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'ritual preserves catastrophe survival knowledge' per the ε-invariance principle. competence_transmission_reading (this file) authors ritual's survival function as embedded technical content, with moderate epsilon (0.48) and a victim set defined by practical-content loss under form retention. symbol_survival_reading authors survival as continuity-of-practice itself, with a structurally different (likely much lower) epsilon since form-preserving communities are that reading's success case, not its victim. hybrid_encoding_reading treats both registers as jointly load-bearing and would author an intermediate epsilon reflecting partial extraction on the practical axis coexisting with intact symbolic function. All three share the same kernel (catastrophe_memory_survival) and are linked bidirectionally; the readings compete for interpretive primacy in religious studies and ethnography without any single framework needing to adjudicate between them a priori.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
