% ============================================================================
% CONSTRAINT STORY: simultaneous_veneration__pragmatic_incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_simultaneous_veneration__pragmatic_incoherence_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: simultaneous_veneration__pragmatic_incoherence_reading
 *   human_readable: Simultaneous Veneration as Suppressed Incoherence (Pragmatic Reading)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   Simultaneous veneration of kami and buddhas in Edo-period and early Meiji
 *   Japan is read here as an institutional arrangement sustained by
 *   suppressed logical incoherence rather than by genuine coordination or
 *   ontological fusion. Practitioners held mutually contradictory beliefs
 *   about the nature and function of kami and buddhas without resolution,
 *   enabled by institutional structures that imposed no enforcement pressure
 *   to choose. When the Meiji state imposed shinbutsu-bunri (separation of
 *   kami and buddhas) after 1868, the constraint was not destroyed by
 *   external rupture—it was revealed as having been unstable and incoherent
 *   all along. The measurement trajectory shows rising extractiveness as
 *   institutional pressure accumulated in the late Edo period, rising
 *   theater_ratio indicating growing performative maintenance of an
 *   incoherent framework, and suppression_requirement remaining high
 *   throughout because the constraint depended on practitioners' inability to
 *   voice or resolve the contradiction.
 *
 * KEY AGENTS:
 *   - Village practitioners (powerless, identity_locked): hold both kami and Buddhist frameworks as true without resolution; exit would require abandoning community religious identity.
 *   - Established Buddhist priesthood (institutional, beneficiary): collects fees from both kami and Buddhist practitioners; maintains authority through honji-suijaku (kami-as-buddha-manifestations) without enforcing coherence.
 *   - Shinto shrine operators (organized, beneficiary): collect fees for kami rituals; maintain authority by performing kami-centered practice without demanding Buddhist rejection.
 *   - Reformist theologians (moderate, payer): bear the cost of unresolved incoherence through theological labor and institutional marginalization.
 *   - Meiji state (institutional, agenda_setter): enforces shinbutsu-bunri as the external mechanism revealing latent incoherence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, 0.72).
domain_priors:suppression_score(simultaneous_veneration__pragmatic_incoherence_reading, 0.68).
domain_priors:theater_ratio(simultaneous_veneration__pragmatic_incoherence_reading, 0.79).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0.79).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(simultaneous_veneration__pragmatic_incoherence_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(simultaneous_veneration__pragmatic_incoherence_reading, snare).
narrative_ontology:human_readable(simultaneous_veneration__pragmatic_incoherence_reading, "Simultaneous Veneration as Suppressed Incoherence (Pragmatic Reading)").
narrative_ontology:topic_domain(simultaneous_veneration__pragmatic_incoherence_reading, "religious_studies/comparative_religion/japanese_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(simultaneous_veneration__pragmatic_incoherence_reading, '6342d247-db99-4172-8653-61be08ae037b').
narrative_ontology:cs_kernel_codification('6342d247-db99-4172-8653-61be08ae037b', implicit).
narrative_ontology:cs_authority_grounding('6342d247-db99-4172-8653-61be08ae037b', practice).
narrative_ontology:cs_interpretation_layer_present('6342d247-db99-4172-8653-61be08ae037b').
narrative_ontology:cs_reading_relation('6342d247-db99-4172-8653-61be08ae037b', simultaneous_veneration__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('6342d247-db99-4172-8653-61be08ae037b', simultaneous_veneration__ontological_fusion_reading, forecloses).
narrative_ontology:cs_axiom('6342d247-db99-4172-8653-61be08ae037b', foundational, simultaneous_veneration_logically_incoherent).
narrative_ontology:cs_axiom_status(simultaneous_veneration_logically_incoherent, holdable).
narrative_ontology:cs_axiom_grounding('6342d247-db99-4172-8653-61be08ae037b', simultaneous_veneration_logically_incoherent, empirically_contingent).
narrative_ontology:cs_axiom('6342d247-db99-4172-8653-61be08ae037b', secondary, institutional_suppression_masks_contradiction).
narrative_ontology:cs_axiom_status(institutional_suppression_masks_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('6342d247-db99-4172-8653-61be08ae037b', institutional_suppression_masks_contradiction, instrumental).
narrative_ontology:cs_reference_frame('6342d247-db99-4172-8653-61be08ae037b', institutional_permission_for_simultaneous_practice).
narrative_ontology:cs_drift_state('6342d247-db99-4172-8653-61be08ae037b', meiji_enforcement_of_separation, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('6342d247-db99-4172-8653-61be08ae037b', '').
narrative_ontology:cs_kernel_id(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration).

% --- Structural relationships ---
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, practitioners_holding_contradictory_beliefs).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, reformist_theologians_pre_meiji).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, established_buddhist_priesthood).
narrative_ontology:constraint_beneficiary(simultaneous_veneration__pragmatic_incoherence_reading, shinto_shrine_operators).
narrative_ontology:constraint_victim(simultaneous_veneration__pragmatic_incoherence_reading, village_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perform rituals to kami for immediate prosperity, health, and protection; simultaneously rely on Buddhist priests for merit-accumulation, afterlife salvation, and ethical instruction. They hold both frameworks as true without recognizing the contradiction—or recognize it but treat it as unsayable, because naming the contradiction would require abandoning community religious identity. Village religious practice is identity-constitutive: to question kami-buddha coherence is to question belonging itself.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, village_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Collects fees and donations from both Buddhist practitioners (merit transfer, ordination, temple services) and practitioners seeking Buddhist legitimation for kami veneration (syncretic rituals, honji-suijaku interpretations). Maintains institutional authority by absorbing kami into Buddhist cosmology (kami-as-bodhisattva-manifestations) without demanding explicit rejection of Shinto; this allows simultaneous extraction from multiple ritual domains. Benefits from the unresolved contradiction because active enforcement pressure to choose would reduce their revenue base and institutional reach.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, established_buddhist_priesthood, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(simultaneous_veneration__pragmatic_incoherence_reading, established_buddhist_priesthood, agenda_setter).

% Collect fees and donations for kami rituals, festival sponsorship, talismans, and purification services. Maintain institutional authority by performing kami-centered practice and justifying dual veneration through domain-partition theory (kami for this-world prosperity) without demanding explicit Buddhist rejection. Benefit pragmatically from simultaneous veneration as long as enforcement pressure to choose remains absent; would lose revenue and institutional legitimacy if forced to position themselves as exclusively Shinto.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, shinto_shrine_operators, beneficiary,
    organized, generational, constrained, regional).

% Attempt to resolve the logical contradiction through syncretist theory (honji-suijaku and other coherence frameworks) or through separation doctrine (exclusivist Buddhist or Shinto positions). They bear the cost of the unresolved incoherence in the form of sustained theological labor, institutional marginalization (because their proposals threaten established institutional interests), and practical inability to enforce doctrinal consistency without state backing. Their solution attempts are either absorbed into the system as additional layers of theory (honji-suijaku becomes official doctrine, defanging separation proposals) or delegitimized as sectarian.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, reformist_theologians_pre_meiji, payer,
    moderate, biographical, constrained, national).

% After 1868, enforces shinbutsu-bunri (separation of kami and buddhas) as state policy. Acts as the external enforcement mechanism that reveals the latent incoherence: the unresolved contradiction cannot be maintained once political authority demands explicit choice and makes enforcement machinery available (legal penalties, temple closure, institutional reorganization). The state apparatus does not create the incoherence—it reveals it by withdrawing institutional permission for simultaneous veneration and enforcing coherence through law.
narrative_ontology:constraint_stakeholder(simultaneous_veneration__pragmatic_incoherence_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(simultaneous_veneration__pragmatic_incoherence_reading, established_buddhist_priesthood).
narrative_ontology:fixing_cost_class(simultaneous_veneration__pragmatic_incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading denies that a coordination function exists. The constraint is presented as coordination (domain specialization via kami/buddha distinction, metaphysical fusion via honji-suijaku), but this reading asserts that no stable coordinating principle binds the two frameworks together—practitioners and institutions collude in avoiding the incoherence rather than solving it. The apparent coordination is theatrical maintenance of suppressed contradiction.
% TRANSFER_FUNCTION: Moves religious authority, institutional legitimacy, and material support (fees, donations, social status, personnel) from village practitioners and reformist theologians to both Buddhist priesthood and Shinto shrine operators, sustained by practitioners' inability to enforce logical consistency because doing so would require abandoning the identity-fused community religious framework that binds them to their villages and social worlds.
% ABSENT_VOICES: Practitioners who recognize the contradiction but fear social consequences for voicing it (would be marked as heretical or disloyal to community); theologians whose coherence proposals threaten institutional interests (both Buddhist and Shinto benefit from unresolved simultaneity); alternative religious frameworks (pure Buddhism, pure Shinto, secular rationalism, Christian monotheism) that would resolve the problem but are suppressed by institutional density and community sanctions. The constraint's survival depends on these voices being structurally unavailable—silenced not by explicit law but by community identity-fusion and institutional power to define what questions are permissible.
% DISAPPEARANCE_RATIONALE: If simultaneous veneration disappeared overnight, one interpretation holds that practitioners would reorganize rapidly around chosen frameworks (Buddhism or Shinto or secular life), suggesting the constraint was a contingent institutional arrangement masking incompatibility (this reading's view). Another interpretation holds that practitioners would experience cognitive and social distress and that institutional reorganization would be painful and contested, suggesting the constraint had become psychologically real despite logical incoherence (the domain_partition and ontological_fusion readings' view). The historical record shows the former: practitioners reorganized relatively rapidly after Meiji separation (many chose Buddhist practice, some Shinto, some secular), suggesting the constraint was never deeply integrated psychologically—only institutionally sustained.
% FOUNDING_PROBLEM: Simultaneous veneration was adopted pragmatically in Heian and early medieval Japan because the cultural and religious landscape incorporated both indigenous kami veneration and imported Buddhist frameworks, and practitioners needed both—immediate protection and prosperity from kami, merit-accumulation and salvation from Buddha. Rather than demand explicit choice, institutional structures evolved to allow both without requiring coherence: Buddhist temples absorbed kami worship, shrine priests adopted Buddhist language, and practitioners moved between the two domains without articulating the logical incompatibility.
% FOUNDING_PROBLEM_CORROBORATION: Meiji-era scholars and modernization historians (from outside the benefiting religious institutions) corroborate that the founding pragmatic need—avoiding forced choice—dissolved once state institutions could enforce alternative frameworks and practitioners no longer needed institutional permission to choose. Buddhist scholars of the Edo and early Meiji periods (Mizuki Kakutei, Tominaga Nakamoto) attest from within the tradition that the incoherence was felt and intellectually problematic; their reform proposals were marginalized because they threatened institutional interests. Contemporary scholars of Japanese religion (Josephson 2002, Hardacre 2003, Thal 2005) corroborate from comparative religious studies that simultaneous veneration required suppression of logical contradiction at the institutional level—what practitioners felt was heterodox or dangerously questioning was actively managed as heretical by institutional authorities.
narrative_ontology:disappearance_verdict(simultaneous_veneration__pragmatic_incoherence_reading, contested).
narrative_ontology:founding_problem_status(simultaneous_veneration__pragmatic_incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(simultaneous_veneration__pragmatic_incoherence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(simultaneous_veneration__pragmatic_incoherence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(simultaneous_veneration__pragmatic_incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(simultaneous_veneration__pragmatic_incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.72 at interval end) because both Buddhist and Shinto institutions benefit from practitioners' inability to demand coherence—they extract authority and material support from the same practitioners across incompatible frameworks. Suppression is high (0.68) because the constraint depends on practitioners NOT recognizing or voicing the logical contradiction; the constraint persists through identity-locking (practitioners cannot exit community religious identity) and institutional density (no alternative frameworks are accessible or legitimate). Theater is the highest metric (0.79) because the constraint is almost entirely performative—the institutions perform coherence (through syncretic theory and specialized domain-function claims) while the underlying incoherence remains suppressed. Accessibility collapse is low (0.41) because alternatives exist (pure Buddhism, pure Shinto, secular frameworks) but are suppressed by institutional and community pressure, not by logical necessity. Resistance is moderate (0.58) because reformist theologians mount coherence attempts, but lack institutional backing to enforce them. The measurement series models the constraint's latent accumulation: extractiveness rises gradually as institutional density increases (Buddhist temple expansion, Shinto formalization), theater ratio plateaus high as performative maintenance stabilizes, and suppression requirement remains elevated throughout because the constraint survives only through practitioners' silence.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional beneficiary seats (Buddhist priesthood, Shinto shrine operators), the constraint is genuinely coordinating: it expands their constituency, provides revenue from multiple ritual domains, and is justified through coherence theories (honji-suijaku or domain specialization). From the powerless practitioner seats and the reformist theologian seats, the constraint operates as extraction sustained by suppression of logical contradiction. The engine should compute divergent types across these seats: the beneficiary seats experience coordination (low d toward extraction), while the target seats experience coercion (high d toward extraction, suppressed resistance). The reformist theologians experience the constraint as a snare because they cannot enforce coherence without institutional backing and their coherence proposals threaten institutional interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Village practitioners: d approaches 1.0 (full target) because they pay through identity-locking (cannot exit without abandoning community identity) and have no arbitrage options. Established Buddhist priesthood: d approaches 0.0 (full beneficiary) because they collect fees from both frameworks and maintain institutional authority without enforcing coherence. Shinto shrine operators: d approaches 0.0 for the same reason. Reformist theologians: d approaches 0.8 (near-target) because they bear the cost of logical incoherence through theological labor and institutional marginalization, while lacking the power to enforce resolution. Meiji state: d is analytical (the state is external to the constraint and reveals it rather than participating in it). The directionality derivation should show asymmetry: low-power practitioners are trapped; institutional beneficiaries face no enforcement pressure; reformers face institutional suppression of their coherence proposals.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading asserts that the founding problem (need for both immediate and ultimate salvation frameworks) died when the Meiji state could enforce alternative institutional arrangements, yet the constraint persisted in suppressed form (as honji-suijaku theory and synthetic practice) until explicit separation was imposed. The constraint exhibits mandatrophy: the institutional purpose it served—avoiding forced choice between incompatible frameworks—became unnecessary when state apparatus made choice enforceable. The Meiji separation revealed this: practitioners reorganized rapidly around chosen frameworks, suggesting they had never coherently held both; the simultaneous-veneration constraint was not maintaining a genuine coordination solution but rather suppressing a logical problem that institutions had an interest in leaving unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppressed_vs_resolved,
    'Were practitioners actively suppressing awareness of the logical contradiction, or were they genuinely unaware of it, or did they treat it as metaphysically resolved through honji-suijaku theory?',
    'Post-Meiji separation ethnography and oral histories: rapid coherence and absence of cognitive distress after forced choice would suggest genuine prior unawareness or active suppression; continued theological attempts to reconcile the frameworks would suggest the contradiction was felt but unresolved.',
    'If suppressed: the constraint was a snare (high extraction sustained by coercion to silence). If unaware: the constraint was Rope or Mountain (genuine coordination). If resolved via theory: the constraint was genuinely Rope (the institutions offered a coherent solution practitioners accepted).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppressed_vs_resolved, empirical, 'Whether incoherence was suppressed, unrecognized, or metaphysically resolved.').

omega_variable(
    institutional_interest_in_incoherence,
    'Did Buddhist priesthood and Shinto operators actively maintain the contradiction to sustain their dual-extraction revenue, or did they pursue coherence theories (honji-suijaku, domain specialization) in good faith?',
    'Institutional archive analysis: if honji-suijaku theory was developed as a deliberate cover for revenue maximization, internal records or theological disputes would show awareness of its inadequacy; if pursued in good faith, theological texts would show genuine attempt at coherence without acknowledgment of instrumental motive.',
    'If active maintenance: the constraint is snare-level extraction sustained by institutional collusion. If good-faith theory: the constraint is Tangled Rope (real coordination offered alongside asymmetric extraction). Either way, the extractiveness remains high, but the mandatrophy assessment shifts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_interest_in_incoherence, empirical, 'Whether institutional benefit from incoherence was deliberate or incidental.').

omega_variable(
    domain_partition_coherence,
    'Could practitioners have genuinely held that kami and buddhas serve functionally distinct domains (this-world vs. afterlife) such that the apparent incoherence dissolves under a domain-specialized reading?',
    'Textual analysis of practitioner statements (diaries, confessions, theological tracts) pre- and post-Meiji: if practitioners explicitly deployed domain-partition logic, the constraint could be reframed as coordination, not incoherence. If domain-partition logic appears only post-hoc or only in institutional pronouncements (not in lay practice), it was a cover story, not a genuine resolution.',
    'This is the sibling reading (domain_partition_reading). If domain partition is coherent to practitioners, this reading is overridden; if it is institutional cover, this reading''s core premise (incoherence) is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_coherence, conceptual, 'Whether domain-partition logic was a genuine coherence mechanism or post-hoc cover story.').

omega_variable(
    meiji_revelation_vs_imposition,
    'Did the Meiji state reveal a pre-existing incoherence (this reading''s premise), or did the state impose coherence on a previously stable pragmatic arrangement (domain_partition and ontological_fusion readings)?',
    'Historical record of pre-Meiji coherence debates and post-Meiji reorganization speed: if theologians pre-Meiji were attempting coherence resolution and practitioners post-Meiji reorganized rapidly without distress, the reading is revelatory; if practitioners expressed shock or resistance to separation and reformist efforts intensified after state imposition, the state imposed choice on a genuinely stable arrangement.',
    'If revelatory: the constraint was never stable; Meiji separation revealed latent incoherence. If imposed: the constraint was Rope or Tangled Rope; Meiji separation created the problem it claimed to solve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meiji_revelation_vs_imposition, empirical, 'Whether Meiji shinbutsu-bunri revealed or created the incoherence.').

omega_variable(
    kernel_reading_contest,
    'Which reading of the simultaneous_veneration kernel is structurally correct: domain_partition (functionally distinct), ontological_fusion (metaphysically identical), or pragmatic_incoherence (never coherent)?',
    'This is the kernel contest itself. Each reading claims a different structural relationship (functional, metaphysical, or logical) and predicts different responses to Meiji separation. The resolution mechanism is comparative: which reading best predicts the historical trajectory of post-separation Japanese religion, theological debates, and practitioner behavior?',
    'The kernel reading contest is unresolvable within this framework—each reading is internally coherent and makes different empirical predictions. The corpus should generate all three readings and let the pattern of evidence accumulate across them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which reading of simultaneous veneration is structurally true.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(simultaneous_veneration__pragmatic_incoherence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(simu_tr_t0, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 0, 0.72).
narrative_ontology:measurement_basis(simu_tr_t0, observed).
narrative_ontology:measurement(simu_tr_t20, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 20, 0.74).
narrative_ontology:measurement_basis(simu_tr_t20, observed).
narrative_ontology:measurement(simu_tr_t40, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 40, 0.76).
narrative_ontology:measurement_basis(simu_tr_t40, observed).
narrative_ontology:measurement(simu_tr_t60, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 60, 0.78).
narrative_ontology:measurement_basis(simu_tr_t60, observed).
narrative_ontology:measurement(simu_tr_t80, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 80, 0.79).
narrative_ontology:measurement_basis(simu_tr_t80, observed).
narrative_ontology:measurement(simu_tr_t100, simultaneous_veneration__pragmatic_incoherence_reading, theater_ratio, 100, 0.79).
narrative_ontology:measurement_basis(simu_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(simu_be_t0, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(simu_be_t0, observed).
narrative_ontology:measurement(simu_be_t20, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(simu_be_t20, observed).
narrative_ontology:measurement(simu_be_t40, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(simu_be_t40, observed).
narrative_ontology:measurement(simu_be_t60, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(simu_be_t60, observed).
narrative_ontology:measurement(simu_be_t80, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 80, 0.71).
narrative_ontology:measurement_basis(simu_be_t80, observed).
narrative_ontology:measurement(simu_be_t100, simultaneous_veneration__pragmatic_incoherence_reading, base_extractiveness, 100, 0.72).
narrative_ontology:measurement_basis(simu_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(simu_su_t0, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(simu_su_t0, observed).
narrative_ontology:measurement(simu_su_t20, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement_basis(simu_su_t20, observed).
narrative_ontology:measurement(simu_su_t40, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(simu_su_t40, observed).
narrative_ontology:measurement(simu_su_t60, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 60, 0.67).
narrative_ontology:measurement_basis(simu_su_t60, observed).
narrative_ontology:measurement(simu_su_t80, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement_basis(simu_su_t80, observed).
narrative_ontology:measurement(simu_su_t100, simultaneous_veneration__pragmatic_incoherence_reading, suppression_requirement, 100, 0.68).
narrative_ontology:measurement_basis(simu_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(simultaneous_veneration__pragmatic_incoherence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(simultaneous_veneration__pragmatic_incoherence_reading, 0.12).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__domain_partition_reading).
narrative_ontology:affects_constraint(simultaneous_veneration__pragmatic_incoherence_reading, simultaneous_veneration__ontological_fusion_reading).

% DUAL FORMULATION NOTE:
% The simultaneous_veneration kernel admits three structurally distinct readings with different ε values and classifications. This constraint (pragmatic_incoherence_reading) models the kernel as a snare—incoherence sustained by suppression. The domain_partition_reading models the same kernel as coordination (functional specialization). The ontological_fusion_reading models it as Rope (genuine metaphysical coherence). All three stories share the same historical phenomenon (Edo-period dual veneration) but differ in what constraint that phenomenon instantiates. The three are linked via network.affects_constraints forming a constraint family: the pragmatic reading upstream-influences the others because it claims the foundational incoherence that the other readings attempt to resolve.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
