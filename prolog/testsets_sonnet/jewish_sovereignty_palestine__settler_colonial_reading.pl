% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionism as Settler-Colonial Displacement Regime
 *   domain: political philosophy / postcolonial theory / nationalism studies
 *
 * SUMMARY:
 *   This story instantiates the settler-colonial reading of the Jewish
 *   sovereignty in Palestine kernel: Zionism is treated as structurally
 *   continuous with European settler-colonial projects (Algeria, South
 *   Africa, Rhodesia, Australia), in which an incoming population, sponsored
 *   by an imperial patron, establishes political and demographic control over
 *   a territory at the expense of an indigenous population — with individual
 *   settler intent (including flight from persecution) treated as
 *   structurally irrelevant to the outcome produced. This is NOT the only
 *   defensible reading of the kernel; the liberal_nationalist_reading,
 *   religious_zionist_reading, cultural_zionist_reading, and
 *   post_zionist_reading are separate constraint stories with different
 *   beneficiary/victim structures and different epsilon values, linked via
 *   network.affects_constraints. This story's epsilon is authored as high and
 *   stable-to-rising because the reading's core claim is that the territorial
 *   logic is zero-sum: land and sovereignty transferred to the settler
 *   population and its patrons cannot simultaneously remain available to the
 *   indigenous population, and this transfer intensified rather than resolved
 *   across 1948, 1967, and the post-Oslo settlement expansion period.
 *
 * KEY AGENTS:
 *   - palestinian_indigenous_population: primary structural victim, powerless/trapped — bears the land and sovereignty transfer
 *   - palestinian_refugees: victim of the displacement's permanent stabilization mechanism (denial of return)
 *   - palestinian_citizens_of_israel: victim within a formally inclusive but structurally subordinating citizenship regime
 *   - jewish_israeli_settler_population: structural beneficiary of the national project despite individual refugee biography — identity-locked exit
 *   - british_mandate_administration: founding agenda-setter and initial imperial beneficiary, later exited without cost
 *   - us_strategic_interests: successor imperial beneficiary, arbitrage-grade exit, bears no territorial cost
 *   - jewish_israeli_landholding_class: concentrated domestic beneficiary of the land-transfer mechanism specifically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.82).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionism as Settler-Colonial Displacement Regime").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political philosophy / postcolonial theory / nationalism studies").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '9fca7ea4-580f-42a9-b53b-feb2a8da0d03').
narrative_ontology:cs_kernel_codification('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', distributed).
narrative_ontology:cs_authority_grounding('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', distributed).
narrative_ontology:cs_reading_relation('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', foundational, structural_outcome_supersedes_settler_intent).
narrative_ontology:cs_axiom_status(structural_outcome_supersedes_settler_intent, holdable).
narrative_ontology:cs_axiom_grounding('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', structural_outcome_supersedes_settler_intent, conventional).
narrative_ontology:cs_axiom('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', foundational, demographic_territorial_control_is_zero_sum).
narrative_ontology:cs_axiom_status(demographic_territorial_control_is_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', demographic_territorial_control_is_zero_sum, empirically_contingent).
narrative_ontology:cs_reference_frame('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', pre_mandate_ottoman_demographic_baseline).
narrative_ontology:cs_drift_state('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', post_oslo_settlement_expansion_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9fca7ea4-580f-42a9-b53b-feb2a8da0d03', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_administration).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, us_strategic_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_israeli_landholding_class).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_israeli_settler_population).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, jewish_israeli_settler_population).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__settler_colonial_reading, settler_colonial_structural_continuity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Pre-1948 majority population of Mandate Palestine, structurally positioned in this reading as the indigenous population subjected to land transfer, demographic engineering, and eventual mass displacement (Nakba). Their agricultural land, village structures, and political institutions were progressively displaced by an incoming settler population backed first by imperial patronage, later by an independent state's military and legal apparatus. Exit was foreclosed by expulsion, refusal of return, and later by occupation and blockade.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_indigenous_population, payer,
    powerless, generational, trapped, regional).

% Descendants of the 1947-49 and 1967 displacements, dispersed across refugee camps in neighboring states and the diaspora, denied return under Israeli law (Law of Return applies to Jewish immigrants, not to them). In this reading they are the structural product of the displacement regime, not an unfortunate side effect of it — their permanent exclusion from the territory is what stabilizes the demographic outcome the constraint exists to produce.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Remained within the 1948 borders and hold formal citizenship, but in this reading experience a structurally subordinated position within a state that legally privileges Jewish national identity (nation-state law, land allocation regimes, planning law). Exit means leaving family, land, and community; voice within the state's institutions is real but bounded by the ethnic-national character of the constitutional order.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Immigrants and their descendants who built sovereign institutions, agricultural collectives, and later state infrastructure in Palestine/Israel. In this reading they occupy the settler position structurally regardless of individual motive (including flight from genocide and persecution) — their national project's territorial and demographic logic is what displaces the indigenous population. Many are themselves refugees from European and Middle Eastern antisemitic violence, which is why this reading treats intent as irrelevant to structural position: the settler-colonial frame reads structural outcome, not biography. Exit from the national project is identity-locked for many by trauma history and lack of an alternative sovereign guarantee.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_israeli_settler_population, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_israeli_settler_population, payer).

% Issued the Balfour Declaration and administered the Mandate (1920-1948), enabling Jewish immigration and land purchase while suppressing Palestinian political organization, in a pattern this reading treats as classic imperial patronage of a settler project to secure regional strategic position (Suez route, oil transit). Withdrew in 1948 leaving the structural displacement machinery in place, bearing no ongoing cost.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_administration, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, british_mandate_administration, beneficiary).

% Inherited and amplified the imperial-patron role after 1948 and especially after 1967, providing military, diplomatic, and financial support that this reading treats as underwriting the displacement regime in exchange for a reliable regional military partner and forward position against Soviet and later regional rivals. Bears none of the territorial or demographic costs and can adjust the relationship without existential consequence.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, us_strategic_interests, beneficiary,
    institutional, civilizational, arbitrage, global).

% Institutions and individuals (Jewish National Fund, development authorities, private landholders) that acquired and administer land expropriated or purchased under asymmetric conditions from Palestinian owners and absentee-owner statutes; in this reading they are the concentrated domestic beneficiaries of the land-transfer mechanism, distinct from the broader Jewish Israeli population's diffuse national benefit.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_israeli_landholding_class, beneficiary,
    organized, generational, mobile, national).

% UN bodies, human rights organizations, and international law scholars who characterize aspects of the occupation and settlement regime as violations of international humanitarian law; their findings are contested by Israel and its patrons and carry no enforcement mechanism, so their voice enters international discourse but not the operative decision structure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_human_rights_bodies, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the political, military, and legal apparatus of a Jewish-majority sovereign state for a population with a documented history of statelessness and genocide, and secures a reliable Western-aligned strategic position in the Eastern Mediterranean for external patrons.
% TRANSFER_FUNCTION: Transfers land, water rights, political sovereignty, and freedom of movement from the indigenous Palestinian population to the incoming and resident Jewish-Israeli population and the institutions that administer settlement, with imperial patrons (Britain, then the United States) receiving strategic and diplomatic returns without bearing displacement costs.
% ABSENT_VOICES: Palestinian refugees denied return have no seat in Israeli domestic politics or in most bilateral negotiations; Palestinian citizens of Israel have formal voice but operate inside a constitutional order this reading treats as structurally weighted against them; international legal bodies issue findings that are heard but not binding.
% DISAPPEARANCE_RATIONALE: If the sovereignty-and-settlement structure were dissolved overnight, the entire territorial, demographic, and legal architecture of the region would have to be renegotiated: land tenure, citizenship, refugee return, and security arrangements would all require reconstruction from a substantially different baseline, and the imperial patronage relationship that has sustained it would lose its regional anchor.
% FOUNDING_PROBLEM: The founding problem as read in this frame is imperial: securing a reliable settler population and strategic foothold in a geopolitically vital region, using Jewish immigration (itself driven by genuine persecution) as the demographic vehicle for a project whose territorial logic required displacing the existing population.
% FOUNDING_PROBLEM_CORROBORATION: Palestinian historians, UN historical inquiries (e.g., UNSCOP-era and subsequent special rapporteur reports), and a strand of Israeli 'new historian' scholarship (Pappé, and more guardedly Morris on the 1948 events) corroborate the displacement mechanics from outside the beneficiary population; mainstream Zionist historiography and Israeli state institutions dispute the settler-colonial characterization and attest instead to a national liberation founding problem — the corroboration is genuinely contested rather than absent, which is why this reading's status is marked contested rather than asserted as settled fact.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.82, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82 at 2024) because the reading's structural claim is that land, water, and political sovereignty move in one direction — from the indigenous population to the settler population and its institutions — and that this transfer has continued and in some respects intensified (settlement expansion, land expropriation mechanisms) rather than stabilized. Suppression is authored high (0.78) because in this reading the arrangement persists through active legal, military, and administrative machinery (military occupation, permit regimes, the nation-state law) rather than through voluntary continued participation by the payer population. Theater ratio is moderate (0.4): the coordination function for the beneficiary population (physical safety, functioning state institutions) is genuinely substantial, not merely performative, but a growing share of legal and diplomatic activity since Oslo functions to manage the international legitimacy problem rather than resolve the underlying displacement, which is why theater rises from 0.2 in 1917 to a plateau near 0.4-0.45 rather than staying negligible.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian populations are coded as targets (d near the full-target end) because the structural relationship the reading identifies is dispossession, and their exit options are trapped or constrained across all three sub-groups. The Jewish Israeli settler population is coded as a structural beneficiary despite frequently also being individually a refugee population fleeing genocide — this reading's central methodological claim is that structural position is read from outcome (land and sovereignty acquired) rather than from individual motive or history, which is why the settler population's directionality sits toward the beneficiary end even though many members experienced profound victimization elsewhere. This is the single most contested move in the reading and is why it is routed through an omega rather than asserted as uncontestable. The imperial patrons (Britain, then the US) are coded as the least constrained beneficiaries: institutional power, arbitrage/mobile exit, no territorial or demographic cost borne.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem status is marked contested rather than dead precisely because in this reading a founding problem still exists in a strong sense (Jewish physical security, given documented history of genocide) even as the reading argues the specific territorial-displacement mechanism used to solve it has outlived any coordination justification and now operates primarily as an entrenched extraction structure defended by military and legal suppression. This prevents the classification from mislabeling the entire arrangement as either pure coordination (ignoring the displacement) or pure extraction (ignoring the genuine security coordination function for the beneficiary population) — hence tangled_rope rather than snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structure_attribution,
    'Is it valid to assign settler/beneficiary structural position to a population (Jewish immigrants, many fleeing genocide and persecution) irrespective of individual intent and biography, purely on the basis of the territorial and demographic outcome their collective project produced?',
    'This is a conceptual/methodological dispute internal to social theory (structuralist versus intentionalist accounts of colonialism) rather than an empirically resolvable question; comparative colonial studies scholarship and critical responses to it (including scholars who reject the settler-colonial frame''s applicability to a persecuted diaspora population) are the relevant literature, but no dataset resolves it.',
    'If structural-outcome attribution is rejected as the governing frame, this entire reading''s beneficiary/victim assignment for the Jewish Israeli population dissolves and the story collapses toward the liberal_nationalist_reading; if accepted, the tangled_rope classification with Jewish Israelis as structural (not moral) beneficiaries holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intent_vs_structure_attribution, conceptual, 'Whether structural position can be assigned independent of settler intent and refugee biography.').

omega_variable(
    metropole_beneficiary_continuity,
    'Does U.S. strategic patronage post-1967 constitute genuine structural continuity with British imperial patronage pre-1948, or are these sufficiently different relationships (alliance versus colonial administration) that treating them as one continuous beneficiary role overstates the parallel?',
    'Comparative analysis of the legal and administrative form of British Mandate control versus U.S.-Israel military aid and diplomatic relations; historians of the U.S.-Israel relationship are divided on how load-bearing the ''imperial patron'' framing is for the post-1967 period specifically.',
    'If the continuity claim fails, the beneficiary structure of this reading needs to distinguish a genuine colonial-administration phase (1917-1948) from a subsequent alliance-based phase with a different extraction logic, which could split this single story into two.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metropole_beneficiary_continuity, conceptual, 'Whether British Mandate patronage and later U.S. strategic support are structurally the same beneficiary role.').

omega_variable(
    displacement_regime_persistence_mechanism,
    'Is the continued high suppression/extractiveness in the post-1993 period best explained by active ongoing displacement (settlement expansion, permit denial, land expropriation) or by inertial maintenance of a largely stabilized 1948/1967 territorial outcome (a piton-like persistence rather than active extraction)?',
    'Time-series data on settlement construction rates, land expropriation orders, and demolition orders in the West Bank and East Jerusalem since Oslo, compared against population and territorial control baselines from 1967 and 1993.',
    'If displacement is actively ongoing rather than inertial, tangled_rope with rising extraction is the correct read; if the mechanism is now primarily inertial defense of a stabilized outcome with limited new territorial transfer, a piton characterization for the post-1993 period specifically would be more accurate and would argue for a lower suppression_requirement trajectory than authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_regime_persistence_mechanism, empirical, 'Whether post-1993 persistence reflects active ongoing extraction or inertial maintenance of a prior settlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1917, 0.35).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1993, 0.74).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2005, 0.79).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.82).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1993, 0.75).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2005, 0.8).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__settler_colonial_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the jewish_sovereignty_palestine kernel, each with a distinct epsilon and beneficiary/victim structure per the epsilon-invariance principle: settler_colonial_reading (this story, tangled_rope, high extraction, Palestinians as primary victims, imperial metropole as primary beneficiary); liberal_nationalist_reading (self-determination frame, likely rope or tangled_rope with a much narrower victim set); religious_zionist_reading (theological grounding, likely tangled_rope or scaffold depending on messianic-versus-pragmatic framing); cultural_zionist_reading (non-sovereigntist, likely closer to rope); post_zionist_reading (achieved-statehood-now-obstructs-equality, likely piton or tangled_rope with a domestic-equality rather than territorial-displacement extraction axis). These are not five measurements of one constraint; they are five structurally distinct constraints sharing a contested kernel, linked here for contamination-propagation and comparative analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
