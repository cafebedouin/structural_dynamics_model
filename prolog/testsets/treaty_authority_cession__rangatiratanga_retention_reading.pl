% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty of Waitangi: Rangatiratanga Retention Reading (Māori Text Authority)
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) was signed in two languages with divergent
 *   semantics. The rangatiratanga_retention_reading interprets the treaty
 *   through the Māori text, applying the contra proferentem doctrine
 *   (ambiguities resolve against the drafting party—the Crown). Under this
 *   reading, 'kāwanatanga' (governance) is limited to Crown management of
 *   Crown affairs; 'tino rangatiratanga' (full chieftainship and
 *   self-determination) is retained by hapū and iwi. The treaty establishes a
 *   partnership requiring Crown consent from Māori before exercise of
 *   authority over Māori lands or peoples. This reading is one of three
 *   competing interpretations of the same treaty kernel: the
 *   crown_cession_reading claims the English text controls and full
 *   sovereignty was ceded; the retrospective_snare_exposure argues the
 *   textual divergence was itself the extraction mechanism. The
 *   rangatiratanga reading is advanced by Māori legal scholarship, Waitangi
 *   Tribunal inquiries, and iwi/hapū sovereignty claims. It is resisted by
 *   Crown courts (historically absolute rejection, now partial recognition),
 *   Crown legislation (parliamentary override without acknowledgment), and
 *   Crown executive (consultation theater without binding consent). The
 *   constraint's measurement series (0–50 interval) captures the shift from
 *   near-total suppression (pre-1975: extractiveness 0.85, suppression 0.92,
 *   near-zero theater) toward partial institutional recognition (post-1975:
 *   extractiveness declining to 0.62, suppression declining to 0.71, theater
 *   rising to 0.48—consultation performance without enforcement). The
 *   claim/metric gap is deliberate and structural: this reading CLAIMS to be
 *   a rope (partnership with mutual obligation) while the authored metrics
 *   describe a constraint that still operates primarily through suppression,
 *   with increasing performative recognition but diminishing actual
 *   enforcement of the partnership principle.
 *
 * KEY AGENTS:
 *   - hapū_iwi_councils: organized actors holding rangatiratanga authority under this reading; benefit from treaty interpretation that recognizes their sovereignty; constrained by lack of enforcement mechanisms against Crown override
 *   - crown_executive: institutional seat holding legislative and administrative power; bears the cost of partnership constraint under this reading but resists through alternative interpretations and statutory override
 *   - māori_landless_communities: powerless victims of historical land alienation schemes that proceeded despite the reading's partnership principle; trapped with no property remedy
 *   - waitangi_tribunal: institutional observer/agenda-setter that advances the rangatiratanga reading through inquiries; advisory authority only, not binding on Crown courts or Parliament
 *   - crown_courts_pre_1975: historical agenda-setter that enforced the crown_cession_reading for 135 years by denying Māori-text authority and contra proferentem application; no longer holds sole interpretive authority but remains resistant to the rangatiratanga reading in many contexts
 *   - legal_scholarship_iwi_focused: analytical observer providing doctrinal and historical corroboration for the reading; legitimizes it beyond institutional beneficiaries
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.62).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.71).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty of Waitangi: Rangatiratanga Retention Reading (Māori Text Authority)").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '902b9e78-c32b-4765-900c-a9f64bc72dfe').
narrative_ontology:cs_kernel_codification('902b9e78-c32b-4765-900c-a9f64bc72dfe', fixed_text).
narrative_ontology:cs_authority_grounding('902b9e78-c32b-4765-900c-a9f64bc72dfe', lineage).
narrative_ontology:cs_interpretation_layer_present('902b9e78-c32b-4765-900c-a9f64bc72dfe').
narrative_ontology:cs_reading_relation('902b9e78-c32b-4765-900c-a9f64bc72dfe', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('902b9e78-c32b-4765-900c-a9f64bc72dfe', treaty_authority_cession__retrospective_snare_exposure, coexists_with).
narrative_ontology:cs_axiom('902b9e78-c32b-4765-900c-a9f64bc72dfe', foundational, maori_text_controls_treaty_meaning).
narrative_ontology:cs_axiom_status(maori_text_controls_treaty_meaning, holdable).
narrative_ontology:cs_axiom_grounding('902b9e78-c32b-4765-900c-a9f64bc72dfe', maori_text_controls_treaty_meaning, conventional).
narrative_ontology:cs_axiom('902b9e78-c32b-4765-900c-a9f64bc72dfe', foundational, contra_proferentem_doctrine_applies_colonial_treaties).
narrative_ontology:cs_axiom_status(contra_proferentem_doctrine_applies_colonial_treaties, holdable).
narrative_ontology:cs_axiom_grounding('902b9e78-c32b-4765-900c-a9f64bc72dfe', contra_proferentem_doctrine_applies_colonial_treaties, deontological).
narrative_ontology:cs_axiom('902b9e78-c32b-4765-900c-a9f64bc72dfe', foundational, tino_rangatiratanga_retained_full).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained_full, holdable).
narrative_ontology:cs_axiom_grounding('902b9e78-c32b-4765-900c-a9f64bc72dfe', tino_rangatiratanga_retained_full, empirically_contingent).
narrative_ontology:cs_axiom('902b9e78-c32b-4765-900c-a9f64bc72dfe', secondary, partnership_requires_ongoing_consent).
narrative_ontology:cs_axiom_status(partnership_requires_ongoing_consent, holdable).
narrative_ontology:cs_axiom_grounding('902b9e78-c32b-4765-900c-a9f64bc72dfe', partnership_requires_ongoing_consent, instrumental).
narrative_ontology:cs_reference_frame('902b9e78-c32b-4765-900c-a9f64bc72dfe', partnership_and_consent_framework).
narrative_ontology:cs_drift_state('902b9e78-c32b-4765-900c-a9f64bc72dfe', contemporary_post_1975_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('902b9e78-c32b-4765-900c-a9f64bc72dfe', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_iwi_councils).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, maori_sovereignty_claim_holders).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, maori_landless_communities).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_excluded_from_consent_processes).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.62 (endpoint) but was 0.85 at the interval start (1840). The constraint operates through translation asymmetry: Crown legislation and executive action proceed on the assumption that Crown authority is unilateral and unconstrained ('kāwanatanga' = full sovereignty), while the Māori text reading denies this and requires partnership consent. The extraction is the seizure of authority and lands that should, under the rangatiratanga reading, have required demonstrated consent. The reason extractiveness declines over the interval is not that the reading gains enforcement, but that the Crown increasingly performs consultation (theater rises from 0.05 to 0.48) while still maintaining legislative override (suppression remains high at 0.71, suppression_requirement was 0.92 pre-1975). This is classic Goodhart drift: the consultation function (originally meant to operationalize partnership) becomes a proxy goal whose performance satisfies political pressure without transferring actual authority. Suppression (0.71) remains high because the partnership reading is actively denied by Crown courts, legislated around by Parliament, and excluded from Crown administrative decision-making absent specific treaty settlement agreements. Accessibility_collapse (0.68) is moderate: the reading is publicly available (legal scholarship, Tribunal reports) but faces barriers to lived institutional adoption—indigenous communities cannot unilaterally enforce it against Crown authority. Resistance (0.73) is high: hapū, iwi, and legal scholars actively resist the Crown's suppression through litigation, political advocacy, and scholarship, but meet institutional inertia and parliamentary supremacy. The theater_ratio (0.48) reflects the institutional shift post-1975: consultation processes, settlement negotiations, Tribunal inquiries, and official acknowledgment of the reading are theatrically prominent while actual constraint on Crown authority remains limited and contingent on negotiated settlement. Measurement series on one shared time grid: every metric is authored at t=0,10,20,30,40,50.
 *
 * PERSPECTIVAL GAP:
 *   From the hapū/iwi seat: the constraint is experienced as a partial victory (constitutional legitimacy gained) combined with structural defeat (enforcement remains contingent on Crown goodwill). From the Crown seat: the constraint is experienced as a political/reputational cost (consultation obligation, settlement costs) but not a legal loss of authority (Parliament remains supreme, override is always available). From the legal-scholarship seat: the constraint is experienced as an intellectual problem (the kernel remains genuinely contested; multiple readings coexist without resolution). From the landless-communities seat: the constraint is experienced as betrayal—the reading validates their dispossession claim while offering no remedy.
 *
 * DIRECTIONALITY LOGIC:
 *   The rangatiratanga reading benefits hapū/iwi (d approaches beneficiary end: they gain interpretive authority, constitutional standing for sovereignty claims, access to negotiated settlements). It constrains the Crown (d approaches target end: unilateral authority is challenged, legislative override is increasingly challenged on treaty grounds). However, the constraint's persistence depends on Crown acceptance (Parliament and courts could reject it entirely), so the Crown's d is complex: the Crown pays a cost in constrained authority and settlement obligations, but retains legislative override and can renegotiate settlements through majority power. Landless communities are victimized by the historical operation of the competing reading (Crown-cession, English-text-controls) and are excluded from the rangatiratanga reading's current benefits (settlements typically involve iwi/hapū governance, not individual restoration). The reading validates their claims but does not remedy their dispossession. d for landless communities is near-full-target: they are harmed by the extraction that preceded the reading's articulation and excluded from its current operation. Suppression is high because the reading is actively suppressed by Crown courts and legislation; only the Tribunal and Māori political mobilization maintain it. Exit options: hapū/iwi are identity-locked (their authority claim is constitutive of their identity; they cannot exit without existential loss); Crown is constrained (can legislate around the reading but at reputational cost); landless communities are trapped (no property remedy available).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits mandatrophy symptoms: the founding problem (textual divergence, unequal bargaining power, translation asymmetry) remains live, but the founding solution (a clear partnership framework with binding enforcement) has not materialized. Instead, the rangatiratanga reading has been institutionalized as advisory (Tribunal inquiries) and performative (consultation processes, settlement negotiations) while Crown unilateral authority persists and Parliament retains override power. The theater_ratio rise from 0.05 to 0.48 marks the shift from denial (pre-1975: the reading was not acknowledged at all) to performance (post-1975: the reading is acknowledged and consulted but not binding). This is mandatrophy classic: the constraint's original mandate (establish a partnership of equals with mutual binding obligation) is increasingly displaced by institutional performance that satisfies political pressure without transferring authority. The founding_problem_status (live) paired with disappearance_verdict (world_rearranges) indicates the constraint is not a natural law or irreducible coordinating mechanism—the institutional order depends on the Crown maintaining override authority. If the partnership reading were enforced, the Crown's unilateral authority would need radical reconstruction. The Crown's sustained legislative resistance (parliamentary supremacy doctrine unchanged) indicates the reading has not achieved settled institutional status. Mandatrophy resolution: either the reading achieves binding constitutional status (courts enforce it against Parliament), or it remains a piton—a historical document whose meaning is debated but whose enforcement is contingent on settlement negotiation and political pressure rather than law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_divergence_intentionality,
    'Was the semantic divergence between English and Māori texts intentional (deliberate Crown strategy to secure Māori agreement to a text that English speakers understood differently) or negligent (honest translation failure)?',
    'Historical research into the treaty drafting process, Crown correspondence, interpreter records, and the negotiation context. Waitangi Tribunal inquiries have reviewed this; Crown archive material remains incompletely declassified.',
    'If intentional, the divergence constitutes fraud and the contract is voidable under principles of misrepresentation—a snare reading becomes accessible. If negligent, contra proferentem applies but fraud claims are weaker. The reading''s genealogy depends on which answer holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_intentionality, empirical, 'Whether the textual divergence was strategic or negligent.').

omega_variable(
    contra_proferentem_scope_colonial_treaties,
    'Does contra proferentem doctrine (a contract canon that ambiguities are construed against the drafting party) apply to colonial treaties, given the context of unequal power and knowledge at signature?',
    'Comparative jurisprudence across indigenous-settler treaty disputes (Canada, Australia, US). International human rights bodies'' rulings on treaty interpretation. New Zealand courts'' adoption or rejection of the doctrine in treaty cases.',
    'If contra proferentem applies, the Māori text reading is the canonical interpretation. If not, the English text or a balanced interpretation framework applies instead, and the rangatiratanga reading loses its foundational doctrinal support.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contra_proferentem_scope_colonial_treaties, conceptual, 'Whether colonial-treaty ambiguities resolve in favor of the non-drafting party (Māori/Crown).').

omega_variable(
    partnership_enforceability_without_parliament,
    'Can the partnership reading be enforced against Crown legislation that explicitly overrides it, or does parliamentary supremacy (a doctrine embedded in Westminster constitutionalism) permit the Crown to unilaterally rewrite the treaty''s terms through statute?',
    'Constitutional law evolution: does New Zealand courts accept a ''higher law'' treaty status that Parliament cannot override? Or does parliamentary supremacy remain absolute? The 1975 Treaty of Waitangi Act made the treaty a legal document, but courts have disagreed on whether legislation can override it.',
    'If courts enforce the partnership reading against Parliament, the rangatiratanga reading becomes binding constraint on Crown authority—a functional rope with real enforcement. If parliamentary supremacy holds, the reading is advisory only, and the constraint becomes a piton: performed in tribunal inquiries and settlement negotiations but ultimately contingent on Crown legislative choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(partnership_enforceability_without_parliament, conceptual, 'Whether the partnership reading can bind Parliament or remains politically contingent.').

omega_variable(
    rangatiratanga_scope_ambiguity,
    'Does ''tino rangatiratanga'' (full chieftainship/self-determination) preserved in the Māori text mean: (a) territorial/governmental sovereignty over lands and peoples, (b) cultural and social authority within a Crown-sovereign state, or (c) a spectrum contingent on negotiated settlement per issue?',
    'Māori language scholarship on rangatiratanga at the time of treaty (1840) vs. contemporary usage. Waitangi Tribunal findings on the scope of rangatiratanga in specific settlement contexts (fisheries, forests, etc.). Hapū and iwi articulation of what rangatiratanga means in their own frameworks.',
    'If (a), the reading supports full Māori sovereignty claims and forecloses Crown unilateral authority—a snare-reading emerges from failed partnership. If (b), the reading supports cultural autonomy within Crown constitutionalism—a limited rope. If (c), the reading is essentially negotiation-open, the partnership is process-based rather than outcome-defined, and extraction depends on Crown good faith in negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatiratanga_scope_ambiguity, conceptual, 'The semantic scope of ''tino rangatiratanga'' in the treaty and in Māori self-understanding.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of the rangatiratanga reading structural (Crown courts, legislation, and administrative power systematically deny it; alternatives are legally barred) or internalized (Māori communities and Pākehā settlement have absorbed Crown-sovereignty framing; the reading faces resistance from within Māori politics and from historical institutional capture)?',
    'Ethnographic and interview research with Māori communities on the lived experience of treaty authority claims. Analysis of legal barriers to the reading (court doctrine, legislation) vs. political/cultural barriers (competing iwi strategies, assimilationist internalization). Comparison of enforcement intensity pre- and post-1975 (when the Tribunal was established).',
    'If suppression is primarily structural, removing the legal barriers (court doctrine change, legislation) would enable the reading to function. If internalized, cultural decolonization work is prerequisite; legal changes alone would not activate the reading''s constraining force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of the rangatiratanga reading is external/legal or internalized/cultural.').

omega_variable(
    kernel_contest_framing,
    'This constraint is one reading of a contested kernel (the treaty itself). Two sibling readings exist: the crown_cession_reading (English text controls, full sovereignty ceded) and the retrospective_snare_exposure (the textual divergence itself is the extraction mechanism). Which reading is ''correct'' depends on how the kernel is framed: as a contract (contra proferentem applies), as a declaration of sovereignty (English text controls), or as a crime (mistranslation for dispossession). The constraint''s ε-value and type depend on which framing holds.',
    'The kernel contest is irresolvable within the authority structures of either reading—it requires a third party (international human rights bodies, comparative constitutional law) or a shift in constitutional foundations (e.g., New Zealand adoption of a written constitution that explicitly grounds itself in partnership and Māori text authority). The Waitangi Tribunal provides a forum for the rangatiratanga reading but cannot override parliamentary supremacy.',
    'The rangatiratanga reading''s persistence as ''rope'' depends on continued institutional investment in the Tribunal, legal scholarship, and Māori political mobilization. If the crown_cession_reading reasserts itself through court doctrine or legislation, the constraint shifts toward piton (historical document, performative consultation, no real enforcement). If the retrospective_snare_exposure gains political traction, this reading is revealed as inadequate remedy for historical extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_framing, conceptual, 'The kernel contest framing and which reading''s ε-value is descriptively true.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(trea_tr_t10, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(trea_tr_t20, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(trea_tr_t30, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(trea_tr_t40, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(trea_tr_t50, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(trea_be_t10, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 10, 0.79).
narrative_ontology:measurement(trea_be_t20, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(trea_be_t30, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(trea_be_t40, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(trea_be_t50, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(trea_su_t10, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 10, 0.88).
narrative_ontology:measurement(trea_su_t20, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 20, 0.81).
narrative_ontology:measurement(trea_su_t30, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(trea_su_t40, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(trea_su_t50, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(treaty_authority_cession__rangatiratanga_retention_reading, 0.18).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, maori_land_alienation_historical_extraction).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal_advisory_authority).

% DUAL FORMULATION NOTE:
% The Treaty of Waitangi kernel is decomposed into three constraint stories, each instantiating a different reading with different ε values and types: (1) rangatiratanga_retention_reading (this file)—Rope of partnership, ε=0.62, Māori text controls via contra proferentem; (2) crown_cession_reading—Mountain or Snare depending on seat, ε=0.05 (beneficiary seat) to 0.90 (victim seat), English text controls, full sovereignty ceded; (3) retrospective_snare_exposure—Snare, ε=0.95, textual divergence itself is the fraud mechanism. The three readings are linked via network.affects_constraints; each carries omega variables documenting the kernel contest and the alternatives that are ruled out or coexist. This decomposition follows ε-invariance: each reading has a stable, internally consistent ε that reflects the structural facts it asserts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, powerless, 0.95).
constraint_indexing:directionality_override(treaty_authority_cession__rangatiratanga_retention_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
