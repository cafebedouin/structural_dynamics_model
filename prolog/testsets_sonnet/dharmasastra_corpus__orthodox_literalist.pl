% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra as Eternal Revealed Truth Requiring Literal Varna/Jati Observance
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This story models the orthodox literalist reading of the Dharmasastra
 *   corpus (Manusmriti and cognate texts) as a single, ε-invariant
 *   constraint: the claim that varna/jati hierarchy is eternal, cosmically
 *   revealed order requiring literal observance, not a historically
 *   contingent social arrangement subject to reinterpretation or abandonment.
 *   This reading is analytically distinct from the reformist_contextual
 *   reading (which treats the ethical core as separable from time-bound caste
 *   prescriptions) and the abolitionist_rejection reading (which denies the
 *   framework any remaining legitimate authority) — those are separate
 *   constraints in this family, not alternative measurements of the same one.
 *   The orthodox literalist reading is authored here with an expansive victim
 *   set and high extraction because literal observance is precisely the
 *   reading that forecloses individual mobility and treats hierarchical
 *   assignment as non-negotiable revealed fact rather than negotiable social
 *   policy.
 *
 * KEY AGENTS:
 *   - brahmin_priesthood: agenda_setter/beneficiary (institutional/arbitrage) — controls interpretation and ritual monopoly
 *   - kshatriya_landholders: beneficiary (powerful/mobile) — political-martial authority legitimated by the order
 *   - upper_caste_householders: beneficiary (moderate/constrained) — informal boundary enforcers and beneficiaries
 *   - dalits: primary target (powerless/trapped) — bears the most severe extraction and exclusion
 *   - shudras: target (powerless/constrained) — service subordination, educational and ritual exclusion
 *   - women_across_varnas: target (powerless/trapped) — lifelong guardianship regardless of varna
 *   - inter_caste_couples: target (powerless/trapped) — bears acute enforcement violence at boundary crossings
 *   - colonial_and_postcolonial_courts: observer (institutional/analytical) — codified then partially dismantled formal sanction
 *   - reform_movements: excluded (organized/constrained) — objections treated as heresy within the orthodox frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.81).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.86).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.81).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra as Eternal Revealed Truth Requiring Literal Varna/Jati Observance").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, '40648390-4b39-4aac-bcda-93b9cf74306f').
narrative_ontology:cs_kernel_codification('40648390-4b39-4aac-bcda-93b9cf74306f', fixed_text).
narrative_ontology:cs_authority_grounding('40648390-4b39-4aac-bcda-93b9cf74306f', lineage).
narrative_ontology:cs_interpretation_layer_present('40648390-4b39-4aac-bcda-93b9cf74306f').
narrative_ontology:cs_reading_relation('40648390-4b39-4aac-bcda-93b9cf74306f', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('40648390-4b39-4aac-bcda-93b9cf74306f', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('40648390-4b39-4aac-bcda-93b9cf74306f', foundational, varna_hierarchy_eternally_revealed).
narrative_ontology:cs_axiom_status(varna_hierarchy_eternally_revealed, holdable).
narrative_ontology:cs_axiom_grounding('40648390-4b39-4aac-bcda-93b9cf74306f', varna_hierarchy_eternally_revealed, theological).
narrative_ontology:cs_axiom('40648390-4b39-4aac-bcda-93b9cf74306f', foundational, caste_prescriptions_non_severable_from_ethical_core).
narrative_ontology:cs_axiom_status(caste_prescriptions_non_severable_from_ethical_core, holdable).
narrative_ontology:cs_axiom_grounding('40648390-4b39-4aac-bcda-93b9cf74306f', caste_prescriptions_non_severable_from_ethical_core, theological).
narrative_ontology:cs_reference_frame('40648390-4b39-4aac-bcda-93b9cf74306f', vedic_cosmic_order_as_revealed_apauruseya_text).
narrative_ontology:cs_drift_state('40648390-4b39-4aac-bcda-93b9cf74306f', post_constitutional_abolition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('40648390-4b39-4aac-bcda-93b9cf74306f', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, kshatriya_landholders).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_householders).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalits).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudras).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_across_varnas).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, inter_caste_couples).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, vedic_revelation_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_as_cosmic_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls interpretation and transmission of the sastra texts, performs the rituals only they are entitled to perform, adjudicates disputes about ritual purity and marriage eligibility, and derives income, land grants, and social deference from being declared the apex of the hierarchy the texts describe. Can reinterpret ambiguous verses at will while insisting the hierarchy itself is fixed.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood, beneficiary).

% Holds political and martial authority validated by the same textual order that subordinates lower varnas to service and labor obligations toward them; benefits from a legitimating cosmology for land control and tribute extraction without personally administering the doctrine.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, kshatriya_landholders, beneficiary,
    powerful, generational, mobile, regional).

% Receives preferential access to education, temple entry, marriage networks, and occupational choice by birth; enforces boundary maintenance informally (commensality rules, marriage vetting) even without formal priestly office, and bears reputational cost within the community for deviating from prescribed observance.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, upper_caste_householders, beneficiary,
    moderate, biographical, constrained, local).

% Assigned to occupations and residential locations deemed polluting under the literal reading, barred from temple entry, well access, and Vedic study, and subject to social and sometimes physical sanction for perceived boundary violation. Birth determines status with no textually sanctioned path of individual mobility; exit requires either conversion, migration, or rejecting the framework's authority outright.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalits, payer,
    powerless, generational, trapped, regional).

% Assigned a service role to the three higher varnas under literal reading, barred from independent Vedic study and many ritual functions, dependent on upper-caste patronage for livelihood; some economic mobility exists but ritual and educational subordination is textually fixed.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudras, payer,
    powerless, generational, constrained, regional).

% Subject to prescriptions (e.g., lifelong guardianship by father, husband, or son; restricted independent ritual and property agency in the literal reading) regardless of varna; exit is foreclosed by economic dependency, social sanction, and the doctrine's own claim that the prescriptions are the woman's dharma, not an external imposition.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_across_varnas, payer,
    powerless, biographical, trapped, regional).

% Faces social ostracism, family and community-sanctioned violence, and loss of caste standing for unions the literal reading classifies as prohibited or polluting; has no recourse within the framework itself, since the framework's legitimacy depends on maintaining the boundary they crossed.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, inter_caste_couples, payer,
    powerless, immediate, trapped, local).

% Historically codified selected sastra prescriptions into personal law (Anglo-Hindu law), then later enacted constitutional and statutory abolition of untouchability and caste discrimination; observes the orthodox literalist reading as a persisting social practice even where it has lost formal legal sanction.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, colonial_and_postcolonial_courts, observer,
    institutional, generational, analytical, national).

% Bhakti, Arya Samaj, Dalit assertion movements, and constitutional framers who argue the hierarchy is a historical accretion rather than eternal revelation; their readings are treated within the orthodox literalist framework as deviation or heresy rather than a legitimate alternative interpretation, so their objection is structurally excluded from the orthodox interpretive process itself.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reform_movements, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahmin_priesthood).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, shared cosmology and social division of labor (ritual, martial, economic, service functions) that historically reduced ambiguity about role, marriage eligibility, and ritual entitlement across a large and diverse population, and offered a genealogy of meaning connecting everyday householder duty to a cosmic order.
% TRANSFER_FUNCTION: Moves ritual authority, land and tribute income, educational access, and social deference from Shudras, Dalits, and women toward the Brahmin priesthood and Kshatriya/upper-caste householders, justified as each party fulfilling their eternally assigned dharma rather than as a transfer.
% ABSENT_VOICES: Dalits, Shudras, and women were historically barred from producing or authoritatively interpreting the sastra texts that govern them; reform and abolitionist movements are treated as external to the tradition's legitimate interpretive community rather than as parties whose objection the tradition must answer within its own terms.
% DISAPPEARANCE_RATIONALE: If literal observance of varna/jati prescription vanished as a socially enforced norm, temple access, marriage markets, occupational entry, and residential patterns organized around caste boundary maintenance would reorganize substantially in regions where the norm is still socially (not merely legally) active; priestly monopoly on ritual function and the associated income and deference would be directly disrupted.
% FOUNDING_PROBLEM: Ancient and classical Brahminical society sought a comprehensive framework unifying cosmology, law, and social order — assigning ritual, economic, and political function within a single coherent account of dharma to reduce disputes over status, obligation, and succession.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox commentators and priestly lineages attest the founding problem (cosmic order requiring textual fidelity) remains live and unchanged. Independent corroboration from outside the beneficiary set is largely negative: the Indian Constitution (Article 17, abolition of untouchability), decades of Dalit-authored scholarship (e.g., Ambedkar's 'Annihilation of Caste'), and comparative historical scholarship on caste's post-Vedic hardening attest that the social-order-maintenance function the literal reading claims is either obsolete, actively harmful, or was never as fixed as the eternal-revelation claim asserts. No corroboration from a source outside the beneficiary set was found affirming the literal reading's founding-problem status as still live.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.81 at interval end) because the literal reading assigns ritual, educational, and economic entitlement by birth with no textually sanctioned mobility mechanism, concentrating benefit in a numerically small priestly and landholding stratum while imposing enduring costs on numerically much larger Shudra and Dalit populations plus women across all varnas. Suppression is authored even higher (0.86) because the reading's persistence has always depended on active enforcement — commensality policing, marriage-market exclusion, temple-access denial, and historically sanctioned violence — not on voluntary participant preference; suppression is not scaled by scope or power in this authoring, consistent with the framework's rule that only extractiveness is scaled downstream. Theater ratio rises across the interval (0.18 to 0.42) reflecting that formal legal sanction for caste discrimination was progressively withdrawn (untouchability abolition, anti-discrimination statutes) even as social enforcement of the literal reading persisted informally — an increasing share of what maintains the boundary is now performative/communal rather than state-backed, which is itself diagnostic of a coordination-cover structure persisting past its formal mandate.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin priesthood's seat, literal observance is a rope: voluntary participation in a shared cosmic order that everyone benefits from knowing their place within. From a Dalit's seat, the identical textual mandate is experienced as enforced, birth-locked extraction with no legitimate exit. The engine's per-seat computation should recover this divergence directly from the declared power/exit/scope data without needing to be told which seat is 'right' — that divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priesthood sits closest to full beneficiary: they administer interpretation, collect ritual income and deference, and can exercise arbitrage-grade interpretive latitude over ambiguous verses while insisting the hierarchy itself is immutable. Kshatriya landholders and upper-caste householders are beneficiaries with less direct administrative control but real captured value (land, marriage-market position, occupational access). Dalits, Shudras, women across varnas, and inter-caste couples are declared victims with trapped-to-constrained exit: their exit options are foreclosed precisely because the doctrine frames their subordination as their own dharma rather than an externally imposed cost, which is what pushes their derived directionality toward the full-target end rather than merely disadvantaged-but-mobile.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (not pure snare) preserves the fact that the corpus does encode a genuine historical coordination function — a shared account of social role, marriage eligibility, and ritual obligation that reduced certain classes of dispute in a large, diverse pre-modern society. Calling it a pure snare would erase that this is a founding_problem with contested but not manufactured origins. But the tangled_rope classification insists on naming the asymmetric extraction riding on that coordination function: the same structure that reduces role-ambiguity for the whole society concentrates entitlement in specific groups and imposes birth-locked, non-negotiable cost on others. Mandatrophy is visible in the founding_problem_status='contested' + disappearance_verdict='world_rearranges' pairing: the coordination problem (role and dispute resolution in a stateless-scale society) has been substantially superseded by modern legal and administrative institutions, yet the literal-observance reading persists through social enforcement rather than functional necessity — exactly the zombie-mandate pattern the mismatch consumer is built to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eternal_revelation_vs_historical_construction,
    'Is the varna/jati hierarchy genuinely apauruseya (non-human-authored, eternally revealed) as the orthodox literalist reading claims, or is it a historically constructed and progressively hardened social technology that acquired retrospective cosmological justification?',
    'Comparative textual-historical scholarship tracing the hardening of varna into hereditary jati across the Dharmasastra corpus''s compilation period (roughly 200 BCE-200 CE and later commentarial accretion), cross-referenced against earlier Vedic-period social mobility evidence.',
    'If historically constructed, the orthodox literalist reading''s core legitimacy claim collapses and the beneficiary-concentration pattern reads as constructed extraction dressed in cosmological language rather than genuine natural/revealed order — this is the central Mountain-vs-Snare ambiguity for any claim of eternal revealed hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eternal_revelation_vs_historical_construction, conceptual, 'Whether varna/jati hierarchy is genuinely eternal revelation or historically constructed and retrospectively cosmologized.').

omega_variable(
    coordination_function_residual_value,
    'Does the literal-observance reading still perform any non-substitutable coordination function in contemporary contexts (e.g., ritual continuity, community cohesion for adherents) that would be lost entirely under the reformist or abolitionist readings, or has that function been fully superseded by modern legal, educational, and economic institutions?',
    'Comparative sociological study of communities/regions where formal caste enforcement has weakened (post-reservation, post-urbanization) versus those where it remains strong: does dispute resolution, marriage stability, or social cohesion measurably degrade in the former?',
    'If no residual non-substitutable function is found, the founding_problem_status should be read as fully ''dead'' rather than merely ''contested,'' strengthening the tangled_rope-toward-snare drift and undermining coordination-cover justification for continued literal observance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_residual_value, empirical, 'Whether the literal reading retains any genuine non-substitutable coordination value today.').

omega_variable(
    committer_framing_selection,
    'Is the orthodox_literalist reading the correct default framing for ''the Dharmasastra corpus'' as commonly invoked in contemporary religious and political discourse, or does treating it as one reading among three (alongside reformist_contextual and abolitionist_rejection) already understate how much the literalist reading has functioned as the historically dominant, state-codified default (via colonial Anglo-Hindu law and subsequent social practice) rather than a mere co-equal position?',
    'Historical-legal analysis of which reading colonial and early postcolonial courts actually codified into enforceable personal law, and comparative weighting of social enforcement prevalence across regions and time periods.',
    'If the orthodox literalist reading was in fact the operative default with outsized institutional backing for much of the interval, its classification as tangled_rope may understate historical extraction severity relative to a period-specific accounting; the ε-invariance principle requires this be resolved by further decomposition (e.g., a colonial-era codified sub-reading) rather than by adjusting this story''s ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_selection, conceptual, 'Whether treating the three readings as co-equal understates the orthodox literalist reading''s historical institutional dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.18).
narrative_ontology:measurement(dhar_tr_t40, dharmasastra_corpus__orthodox_literalist, theater_ratio, 40, 0.22).
narrative_ontology:measurement(dhar_tr_t80, dharmasastra_corpus__orthodox_literalist, theater_ratio, 80, 0.28).
narrative_ontology:measurement(dhar_tr_t120, dharmasastra_corpus__orthodox_literalist, theater_ratio, 120, 0.34).
narrative_ontology:measurement(dhar_tr_t160, dharmasastra_corpus__orthodox_literalist, theater_ratio, 160, 0.38).
narrative_ontology:measurement(dhar_tr_t200, dharmasastra_corpus__orthodox_literalist, theater_ratio, 200, 0.42).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(dhar_be_t40, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(dhar_be_t80, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(dhar_be_t120, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 120, 0.8).
narrative_ontology:measurement(dhar_be_t160, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 160, 0.8).
narrative_ontology:measurement(dhar_be_t200, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 200, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(dhar_su_t40, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 40, 0.79).
narrative_ontology:measurement(dhar_su_t80, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 80, 0.82).
narrative_ontology:measurement(dhar_su_t120, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 120, 0.86).
narrative_ontology:measurement(dhar_su_t160, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 160, 0.85).
narrative_ontology:measurement(dhar_su_t200, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 200, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__orthodox_literalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__orthodox_literalist, 0.08).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the dharmasastra_corpus kernel. orthodox_literalist (this file) claims eternal, non-severable revealed hierarchy; reformist_contextual claims the ethical core of dharma is separable from time-bound caste prescriptions; abolitionist_rejection denies the framework any remaining legitimate authority. Each reading has its own ε, beneficiary/victim set, and classification per the ε-invariance principle — they are not three measurements of one constraint but three structurally distinct constraints sharing a textual kernel. orthodox_literalist forecloses reformist_contextual's severability premise; it coexists with abolitionist_rejection as a mutually exclusive but simultaneously live public position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
