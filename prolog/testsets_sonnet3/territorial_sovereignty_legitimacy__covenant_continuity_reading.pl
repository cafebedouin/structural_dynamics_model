% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Covenant-Continuity Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the covenant-continuity reading of the contested
 *   territorial sovereignty legitimacy kernel: the claim that Jewish
 *   sovereignty over the territory is legitimate because it derives from an
 *   ancient covenantal promise, sustained through continuous (if
 *   demographically variable) Jewish presence, and confirmed rather than
 *   created by modern international acts (Balfour, Partition, 1948). Under
 *   this reading, the temporal scope of the legitimacy claim extends to the
 *   biblical period, the claim survives long periods of demographic minority
 *   or absence, the 1947 Partition is read as a compromise of a pre-existing
 *   right rather than the origin of a new one, and settlement activity is
 *   framed as return rather than colonization. This is ONE of three
 *   structurally distinct readings of the same kernel; the self-determination
 *   reading (grounding legitimacy in 19th-20th century demographic majority
 *   and residence) and the existential-matrix reading (grounding legitimacy
 *   in survival-necessity rather than juridical history) are separate
 *   constraints with their own ε values, not alternate measurements of this
 *   one.
 *
 * KEY AGENTS:
 *   - israeli_state_institutions: administers and enforces the legitimacy chain through land, settlement, and residency policy
 *   - jewish_state_citizens: primary beneficiaries of settled sovereignty and diplomatic backing
 *   - religious_zionist_settlement_movement: identity-fused agenda-setting beneficiary whose project depends structurally on the covenant claim
 *   - palestinian_residents_of_contested_territory: bear the transfer of land and mobility rights under military administration
 *   - palestinian_refugees_and_descendants: bear the denial of return under a reading that treats 1948 as ratification, not contested founding
 *   - international_recognition_bodies: treated within this reading as having ratified rather than created the right, weakening their standing to revise it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.62).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Covenant-Continuity Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e731b91d-9b81-4181-90fe-c70cf7415e33').
narrative_ontology:cs_kernel_codification('e731b91d-9b81-4181-90fe-c70cf7415e33', distributed).
narrative_ontology:cs_authority_grounding('e731b91d-9b81-4181-90fe-c70cf7415e33', lineage).
narrative_ontology:cs_interpretation_layer_present('e731b91d-9b81-4181-90fe-c70cf7415e33').
narrative_ontology:cs_reading_relation('e731b91d-9b81-4181-90fe-c70cf7415e33', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e731b91d-9b81-4181-90fe-c70cf7415e33', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('e731b91d-9b81-4181-90fe-c70cf7415e33', foundational, covenantal_promise_confers_perpetual_title).
narrative_ontology:cs_axiom_status(covenantal_promise_confers_perpetual_title, holdable).
narrative_ontology:cs_axiom_grounding('e731b91d-9b81-4181-90fe-c70cf7415e33', covenantal_promise_confers_perpetual_title, theological).
narrative_ontology:cs_axiom('e731b91d-9b81-4181-90fe-c70cf7415e33', foundational, continuous_presence_survives_demographic_minority).
narrative_ontology:cs_axiom_status(continuous_presence_survives_demographic_minority, holdable).
narrative_ontology:cs_axiom_grounding('e731b91d-9b81-4181-90fe-c70cf7415e33', continuous_presence_survives_demographic_minority, conventional).
narrative_ontology:cs_axiom('e731b91d-9b81-4181-90fe-c70cf7415e33', secondary, international_recognition_ratifies_rather_than_creates_right).
narrative_ontology:cs_axiom_status(international_recognition_ratifies_rather_than_creates_right, holdable).
narrative_ontology:cs_axiom_grounding('e731b91d-9b81-4181-90fe-c70cf7415e33', international_recognition_ratifies_rather_than_creates_right, conventional).
narrative_ontology:cs_reference_frame('e731b91d-9b81-4181-90fe-c70cf7415e33', covenantal_promise_as_perpetual_title).
narrative_ontology:cs_drift_state('e731b91d-9b81-4181-90fe-c70cf7415e33', post_1993_oslo_and_post_2005_disengagement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e731b91d-9b81-4181-90fe-c70cf7415e33', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_state_citizens).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_residents_of_contested_territory).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees_and_descendants).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, covenantal_promise_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, continuous_presence_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_ratifies_prior_right_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers sovereignty claims, land registry, settlement authorization, and military control over contested territory, citing covenant, continuous presence, and the Balfour/Partition/1948 sequence as the legitimating chain. Sets policy on annexation, settlement expansion, and residency status for non-citizens within the territory it controls.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Hold full citizenship, property, and residency rights across territory administered under this legitimacy claim. Benefit from state protection, land access, and international diplomatic backing that treats the claim as settled. Can emigrate but rarely need to; the constraint underwrites the stability of their daily life.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_state_citizens, beneficiary,
    organized, generational, mobile, national).

% Builds and defends settlements in the West Bank framed explicitly as return to covenantal land rather than new acquisition. Their identity and life project are fused to the covenant-continuity claim; abandoning the claim would dissolve their reason for being where they are. They actively lobby state institutions to expand the territorial reading of the covenant.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlement_movement, agenda_setter).

% Live under military administration, settlement expansion, and land expropriation justified by the covenant-continuity legitimacy chain. Bear checkpoint delays, permit regimes, and demolition orders. Cannot leave the territory without surrendering residency status, and cannot contest the legitimacy claim through any forum that has power to reverse it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_residents_of_contested_territory, payer,
    powerless, biographical, trapped, local).

% Displaced in 1947-49 and after, denied return under a legitimacy framework that treats 1948 establishment as the ratification of a pre-existing covenantal right rather than a contested founding. Hold UNRWA registration and inherited refugee status across multiple states with no state party recognizing a return claim that would unwind the covenant-continuity reading.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugees_and_descendants, payer,
    powerless, generational, trapped, regional).

% UN bodies and signatory states that issued the Partition Plan and subsequent recognitions are treated by this reading as having ratified a right that predates them, not as the source of the right. Their later resolutions questioning settlement legality are read by this constraint's proponents as departures from their own founding act, giving them structurally weak standing to revise the claim within this reading's own logic.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, excluded,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_recognition_bodies, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, non-arbitrary basis for a state's territorial claim by chaining religious-historical continuity to modern legal recognition, allowing citizens, institutions, and allied states to coordinate around a single settled narrative of legitimate sovereignty rather than relitigating it perpetually.
% TRANSFER_FUNCTION: Moves land tenure, residency security, freedom of movement, and diplomatic legitimacy from Palestinian residents and refugees toward Israeli state institutions and Jewish citizens/settlers, using the covenant-continuity chain to characterize that transfer as restoration rather than dispossession.
% ABSENT_VOICES: Palestinian residents and refugees would object that the covenant claim erases their own multi-generational presence and treats 1948 and its aftermath as ratification rather than as the origin of a genuinely contested and violent founding; they are structurally outside the legitimacy chain this reading authors, since the chain's temporal logic (biblical continuity) categorically cannot include their claim on equal terms.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy claim were withdrawn overnight as a basis for policy, settlement expansion premised on 'return' would lose its justificatory language, annexation arguments resting on pre-existing right rather than negotiated acquisition would collapse, and territorial questions would have to be renegotiated on other grounds (security, self-determination, or existential-survival arguments) — the settlement movement's project in particular has no other stated foundation.
% FOUNDING_PROBLEM: The problem this reading was built to solve: establishing a legitimacy basis for Jewish sovereignty in the territory that would not depend solely on 1947-48 diplomatic and military events (which were themselves contested and incomplete), by anchoring the claim in an older, allegedly unbroken chain of promise and presence that modern recognition merely confirms rather than creates.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist authorities and Israeli state historiography attest the covenant claim as living and foundational. Independent historians (including Israeli New Historians such as Benny Morris and Ilan Pappé) and international legal scholars outside the beneficiary set attest that the demographic and legal record of 1947-49 is contested on its own modern-legal terms and that the covenantal framing functions to insulate the territorial claim from that contestation rather than resolve it; UN bodies outside the benefiting parties have repeatedly declined to treat the covenant claim as a valid basis for settlement legality.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects substantial but not maximal transfer: some coordination function is genuinely served (stable governance, a basis other than raw conquest for claims recognized by many states), but land, residency, and mobility are being moved from a powerless, trapped population toward an organized, institutionally backed one, with the transfer justified retroactively rather than negotiated. Suppression (0.62) is high because occupation-era administrative and military mechanisms (permits, checkpoints, demolition orders) actively foreclose Palestinian alternatives; it spikes around 1967 with the acquisition of the West Bank and Gaza and remains elevated rather than falling, reflecting an enforcement apparatus that hardened rather than wound down. Theater ratio is low-to-moderate (0.28 by 2024) — the security and administrative functions are largely real, not merely performative, though the proportion of activity dedicated to sustaining the covenant framing itself (versus addressing present security needs) has grown. Accessibility collapse (0.45) and resistance (0.78) reflect that this is emphatically not a natural-law constraint: alternatives (negotiated partition, binational arrangements, internationalized status) remain conceptually and diplomatically available and are actively pursued by many parties, and the claim meets sustained, organized resistance from those it costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and jewish_state_citizens sit near the beneficiary end: the legitimacy chain underwrites their security of tenure and diplomatic standing at low direct cost to them. The religious_zionist_settlement_movement is a beneficiary whose exit option is identity-locked rather than merely mobile — abandoning the covenant claim would dissolve the movement's reason for occupying particular land, which is a stronger form of directional commitment than ordinary beneficiary status. palestinian_residents_of_contested_territory and palestinian_refugees_and_descendants sit at the target end: trapped exit options, powerless power atom, and a legitimacy chain whose temporal logic (biblical continuity) is structurally incapable of accommodating their own multi-generational presence claim on the same terms — this is not an oversight but a structural feature of how this reading is built.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing sovereignty legitimacy independent of contested 1947-49 events — is contested rather than dead: it remains actively invoked by the settlement movement and state institutions (status: live within the beneficiary tradition), while corroborating sources outside that tradition (New Historians, international legal bodies) argue the underlying diplomatic-legal problem it was meant to settle was never actually resolved on modern legal terms, only rhetorically bypassed by appeal to covenant. This produces a genealogy mismatch: founding_problem_status is contested while the disappearance_verdict is world_rearranges — the classic capture-flag signature, since a live-but-contested founding claim persisting through active enforcement, with organized beneficiaries and trapped payers, is precisely the tangled_rope pattern rather than either pure coordination or a resolved mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_as_legitimating_fact_vs_retroactive_construction,
    'Is the covenantal claim a genuine continuous legitimacy chain recognized (even implicitly) across the historical record, or is it a retroactive theological framing constructed to give modern territorial claims a legitimacy that the 1947-49 diplomatic and military record alone cannot fully supply?',
    'Comparative historiographical and legal analysis of how continuity claims of this type are treated in other territorial disputes (e.g. indigenous land claims, restitution claims after centuries of displacement) versus how international law actually treats prescription, recognition, and demographic change as sources of title.',
    'If the covenant functions primarily as retroactive legitimation for a claim whose actual legal basis is the modern diplomatic-military sequence, then this reading''s structural function shifts from disclosing a pre-existing right to manufacturing legitimacy for a contested modern outcome — strengthening the tangled_rope classification and weakening any Mountain-adjacent naturalization of the claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_as_legitimating_fact_vs_retroactive_construction, conceptual, 'Whether covenant continuity is a genuine legitimacy chain or a retroactive construction over a modern contested founding.').

omega_variable(
    demographic_absence_survival_rule,
    'Under what conditions does this reading hold that a legitimacy claim survives centuries of demographic minority or absence, and is that survival rule applied consistently or asymmetrically relative to the same rule''s implications for other claimants (e.g. Palestinian residence continuity, or other diasporic return claims globally)?',
    'Cross-case comparison of how continuity-survives-absence claims are adjudicated in comparable disputes, and whether the rule as stated would, if applied symmetrically, also validate claims this reading currently treats as foreclosed.',
    'If the survival rule is applied asymmetrically (validating this claim''s absence-spanning continuity while denying the same logic to Palestinian presence-and-displacement claims), that asymmetry is itself evidence the rule functions as advocacy scaffolding rather than a neutral juridical principle — relevant to whether this reading should be read as tangled_rope (real coordination function plus asymmetric extraction) or closer to pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_absence_survival_rule, conceptual, 'Whether the demographic-absence-survives rule is applied symmetrically across claimant groups.').

omega_variable(
    international_recognition_ratification_vs_origination,
    'Do the Balfour Declaration, Partition Plan, and 1948 establishment function within this reading as ratifications of a pre-existing right, or are they better understood (including by many of the same international actors who issued them) as the actual origin of the legal claim, with the covenant serving only as supporting narrative rather than jurisprudential source?',
    'Textual and diplomatic-historical analysis of the instruments themselves and the stated intent of the issuing bodies (League of Nations Mandate documents, UN General Assembly debate records) regarding whether they understood themselves to be creating, recognizing, or merely administering a right.',
    'If the issuing bodies understood themselves to be creating a new arrangement rather than ratifying a prior covenantal right, then the international-recognition leg of this reading''s legitimacy chain is doing more independent work than the covenant leg, which changes how the reading should be weighted relative to the self_determination_reading it competes with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_ratification_vs_origination, empirical, 'Whether Balfour/Partition/1948 ratified a prior right or originated the claim independent of covenant.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1993, 0.22).
narrative_ontology:measurement(terr_tr_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.32).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1993, 0.52).
narrative_ontology:measurement(terr_be_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2005, 0.55).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.35).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.68).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(terr_su_t2005, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.1).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the territorial_sovereignty_legitimacy kernel. covenant_continuity_reading grounds legitimacy in an ancient-covenant-plus-continuity-plus-modern-recognition chain and extends legitimacy's temporal scope to the biblical period. self_determination_reading grounds legitimacy in modern (19th-20th century) demographic majority and residence, producing a structurally opposed victim/beneficiary assignment. existential_matrix_reading treats legitimacy as existential-survival rather than juridical, sidestepping the historical-priority contest entirely. Each reading is authored as its own constraint with its own ε, beneficiaries, and victims; they are linked here via affects_constraints rather than merged, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
