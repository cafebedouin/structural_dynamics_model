% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Treaty Bindingness — Graduated Compliance Reading
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   The JCPOA (Joint Comprehensive Plan of Action, 2015) represents a
 *   negotiated settlement between Iran and the P5+1 (US, UK, France, Russia,
 *   China, Germany) on nuclear enrichment, sanctions relief, and
 *   verification. This constraint story models ONE reading of the treaty: the
 *   graduated_compliance_reading, which frames the JCPOA as a scaled
 *   reciprocal commitment in which enforcement intensity is proportional to
 *   violation severity, rather than binary (full enforcement or full
 *   sanctions relief). In this reading, the treaty bindingness derives from
 *   reciprocal obligation — each party commits to proportional responses, and
 *   the framework enables de-escalation when violations are remedied
 *   proportionally. The constraint exhibits Tangled Rope structure: genuine
 *   coordination function (sanctions relief synchronized with enrichment
 *   reduction, verification enables trust), but also asymmetric extraction
 *   (NPT-doctrine adherents bear legitimacy cost, non-signatories face
 *   precedent undermining zero-enrichment norm, verification asymmetries
 *   favor the P5+1). Beneficiaries are pragmatic diplomacy advocates (who see
 *   graduated response as enabling engagement) and secondary-market economic
 *   actors (who benefit from sanctions relief). Victims are strict
 *   non-proliferation doctrine adherents (who see any enrichment tolerance as
 *   normative erosion) and verification transparency interests (who bear the
 *   cost of ambiguous compliance thresholds). The measurements show
 *   increasing extractiveness and suppression over the constraint's first 8
 *   years (2015–2023), with theater ratio rising as proportionality
 *   calibration disputes emerged (US withdrawal 2018, Iranian enrichment
 *   acceleration 2019–2021, snapback discretion questions 2022–2023). This
 *   reading coexists with two sibling readings: binding_multilateral_reading
 *   (the JCPOA as universal law binding all states), and
 *   transactional_provisional_reading (the JCPOA as contingent deal
 *   terminating with signatory turnover). This story models the middle
 *   position: the constraint is binding through reciprocal commitment and
 *   practiced obligation, but the binding mechanism is behavioral rather than
 *   absolute legal force.
 *
 * KEY AGENTS:
 *   - Iran (Powerful/Mobile): Primary beneficiary of sanctions relief; primary target of verification regime; mixed experience of coordination and constraint
 *   - Pragmatic Diplomacy Coalition (Institutional/Arbitrage): EU, Russia, China, professional mediators — benefit from graduated compliance flexibility and face-saving de-escalation logic
 *   - P5+1 Nuclear Powers (Institutional/Arbitrage): Signatories to the agreement; benefit from verification access and sanctions control mechanisms
 *   - GCC and Israel (Moderate/Constrained): Regional security stakeholders constrained by verification asymmetries but also benefit from transparency and early warning
 *   - NPT Doctrine Advocates (Powerless/Trapped): Strict non-proliferation advocates in non-signatory states bearing legitimacy cost of enrichment tolerance precedent
 *   - IAEA Verification Regime (Organized/Constrained): International institutional actor with temporary sunset (inspection restrictions phase out in 15 years from agreement)
 *   - NPT Treaty Authority (Institutional/Arbitrage): Formal non-proliferation regime displaced from functional authority but maintaining normative reference role (Piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.52).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.48).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Treaty Bindingness — Graduated Compliance Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '488ebd90-c8b9-41d6-8423-0f69189d80c7').
narrative_ontology:cs_kernel_codification('488ebd90-c8b9-41d6-8423-0f69189d80c7', fixed_text).
narrative_ontology:cs_authority_grounding('488ebd90-c8b9-41d6-8423-0f69189d80c7', extraction).
narrative_ontology:cs_interpretation_layer_present('488ebd90-c8b9-41d6-8423-0f69189d80c7').
narrative_ontology:cs_reading_relation('488ebd90-c8b9-41d6-8423-0f69189d80c7', jcpoa_treaty_bindingness__binding_multilateral_reading, influences).
narrative_ontology:cs_reading_relation('488ebd90-c8b9-41d6-8423-0f69189d80c7', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('488ebd90-c8b9-41d6-8423-0f69189d80c7', foundational, proportional_enforcement_feasible).
narrative_ontology:cs_axiom_status(proportional_enforcement_feasible, holdable).
narrative_ontology:cs_axiom_grounding('488ebd90-c8b9-41d6-8423-0f69189d80c7', proportional_enforcement_feasible, empirically_contingent).
narrative_ontology:cs_axiom('488ebd90-c8b9-41d6-8423-0f69189d80c7', foundational, reciprocal_commitment_binds_without_absolute_legal_force).
narrative_ontology:cs_axiom_status(reciprocal_commitment_binds_without_absolute_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('488ebd90-c8b9-41d6-8423-0f69189d80c7', reciprocal_commitment_binds_without_absolute_legal_force, deontological).
narrative_ontology:cs_reference_frame('488ebd90-c8b9-41d6-8423-0f69189d80c7', reciprocal_graduated_commitment_with_proportional_enforcement).
narrative_ontology:cs_drift_state('488ebd90-c8b9-41d6-8423-0f69189d80c7', post_2018_us_withdrawal, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('488ebd90-c8b9-41d6-8423-0f69189d80c7', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, secondary_market_economic_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_partial_sanctions_relief_beneficiaries).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, strict_non_proliferation_doctrine_adherents).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, states_excluded_from_negotiations).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, verification_transparency_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NPT DOCTRINE / NON-SIGNATORIES (SNARE) — Trapped by a bilateral framework that does not include them and cannot exit the verification asymmetry it creates. The graduated compliance logic implies tolerance for enrichment increases as long as they remain 'proportional' — a precedent that undermines the non-negotiable zero-enrichment position of strict non-proliferation doctrine. These actors bear the cost of diplomatic legitimacy being extended to a nuclear threshold actor.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GCC / ISRAEL (TANGLED ROPE) — Constrained by the verification framework's limitations (sampling access, compliance timelines) but also benefit from transparency mechanisms and early warning. Experience both coordination (knowing Iran's enrichment trajectory) and extraction (commitment to reciprocal restraint while Iran maintains regional leverage). Significant agency through intelligence capabilities and diplomatic coalitions, but cannot unilaterally exit the verification regime.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRAGMATIC DIPLOMACY (ROPE) — Experiences the constraint as coordination: graduated compliance logic enables face-saving de-escalation when violations occur. Can arbitrage between partial sanctions relief and monitoring intensity. Sees violations as tractable through proportional response rather than binary enforcement. Net beneficiary of the constraint's flexibility — maintains diplomatic legitimacy and economic engagement momentum.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: IRAN (TANGLED ROPE) — Powerful actor with mobile exit options (can escalate enrichment, withdraw from agreement, pursue covert pathways) but also genuine beneficiary of partial sanctions relief and normalized trade access. Experiences the constraint as mixed: coordination of sanctions relief and economic re-engagement, but also asymmetric verification (IAEA access exceeds precedent in any prior agreement). Incentive structure ties sanctions relief to compliance — graduated framework allows recalibration of both.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: IAEA / VERIFICATION REGIME (SCAFFOLD) — Organized institutional actor with constrained exit (cannot withdraw verification mandate without state action) but with a clear sunset structure: IAEA inspection restrictions phase out over 15 years. The graduated compliance framework is temporary scaffolding for learning whether Iran will voluntarily sustain non-enrichment beyond the formal compliance period. Sees the constraint as transitional — the endpoint is either nuclear détente (sunset) or enforcement escalation (enforcement pathway).
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NPT TREATY AUTHORITY (PITON) — The formal nuclear non-proliferation regime sees the graduated compliance reading as theatrical performance: the JCPOA's proportional enforcement logic deviates from NPT precedent (which treats violation severity equally) but claims to be consistent with NPT principles. The theater here is in framing a flexible bilateral engagement as instantiation of universal treaty law. The NPT authority has lost functional authority over the constraint (the JCPOA operates independently) but persists as a normative reference point. Theater ratio is high because the performance of 'staying consistent with NPT' obscures the structural deviation from NPT's binary enforcement model.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, reciprocal commitment structures with graduated enforcement are immutable features of treaty law: any binding agreement between unequal parties requires proportional calibration or it becomes self-defeating. The constraint is seen as a natural outgrowth of rational negotiation under conditions of mutual suspicion. However, this obscures that 'proportionality' is a contested interpretive claim — different parties disagree on whether enrichment increases are proportional to sanctions reductions. This perspective risks naturalizing what is actually a negotiated compromise as inherent principle.
constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jcpoa_treaty_bindingness__graduated_compliance_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, TR),
    TR >= 0.70.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes asymmetric verification obligations on Iran (Complementary Access, Additional Protocol, past military dimensions investigation exceeding NPT precedent) and allows P5+1 to maintain sanctions control mechanisms. Simultaneously, Iran gains partial sanctions relief and normalized trade access. The net extractiveness reflects that the P5+1 retain disproportionate capacity to reimpose sanctions (unilateral withdrawal, as Trump administration demonstrated) while Iran's enrichment is transparently monitored. However, the extractiveness is calibrated rather than absolute — graduated compliance logic implies that proportional Iranian violations receive proportional (not total) sanctions reimposition, which reduces extraction compared to binary enforcement. Extractiveness rises over the interval as US withdrawal (2018) and Iranian acceleration (2019–2021) created ambiguity about whether proportional response would actually occur, increasing the constraint's practical extractiveness. Suppression (0.48): Moderate. Suppression mechanisms include verification restrictions (Iran cannot refuse IAEA sampling), reputational cost of violation (undermines legitimacy of P5+1 engagement narrative), and sanctions reimposition threat. However, suppression is not total — Iran has genuine alternatives (covert enrichment, enrichment acceleration, JCPOA withdrawal) and has exercised them when violations were suspected. Suppression rises over the interval as dispute resolution became more acrimonious (US withdrawal 2018 triggered Iranian retaliation 2019, converting suppression from monitoring cost into sanctions reimposition threat). Theater ratio (0.58): Moderate-high. The constraint exhibits significant theatrical performance: (A) Proportionality claims are ambiguous — no agreed metric for what constitutes 'proportional' response to enrichment increases; (B) Snapback mechanism is described as automatic but operates through discretionary UN Security Council consensus non-renewal; (C) NPT authority framing naturalizes a bilateral agreement as universal treaty principle. Theater rises over the interval as the gap between 'graduated compliance' rhetoric and actual enforcement (US withdrawal short-circuiting proportional response) became visible. Theater reflects the constraint's reliance on performative legitimacy — parties frame their actions as 'proportional' or 'defensive' rather than acknowledging them as political choices.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The pragmatic diplomacy coalition (Rope perspective) sees graduated compliance as enabling face-saving de-escalation and sustained engagement. Iran (Tangled Rope) experiences both coordination benefits (sanctions relief, normalized trade) and constraint costs (intrusive verification, sanctions reimposition threat). GCC/Israel (Tangled Rope) see coordination value (transparency, early warning) and extraction cost (forced commitment to restraint while Iran maintains regional leverage). Strict NPT doctrine advocates (Snare) see pure extraction — the precedent of enrichment tolerance undermines zero-enrichment norm regardless of whether Iran abides by JCPOA terms. The IAEA (Scaffold) sees temporary scaffolding — the verification regime is inherently transitional (inspection restrictions sunset in 15 years), creating an implicit de-escalation endpoint. The NPT authority (Piton) sees degraded ritual — the formal treaty regime has lost functional authority (the JCPOA operates independently) but persists as normative reference. The analytical observer (Mountain/false summit) risks naturalizing the constraint as inevitable feature of reciprocal treaty law, obscuring that 'proportionality' is a contested interpretive claim without objective metric. The perspectival gaps reflect deep structural disagreements: Is this constraint binding or provisional? Is proportionality objective or negotiated? Is verification reciprocal or asymmetric?
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation computes d (the agent's structural position relative to the constraint) from beneficiary/victim status and exit options. Iran (powerful/mobile) derives d ≈ 0.45–0.55 (benefits from sanctions relief but faces verification intrusion; can exit through enrichment acceleration or withdrawal). Pragmatic diplomacy coalition (institutional/arbitrage) derives d ≈ 0.15–0.25 (benefits from engagement sustainability and sanctions control; high exit capacity through diplomatic reframing). NPT doctrine advocates (powerless/trapped) derive d ≈ 0.95 (trapped by precedent effect, cannot exit, bear normative cost). GCC/Israel (moderate/constrained) derive d ≈ 0.60–0.70 (constrained by verification limitations and forced regional commitment; moderate exit capacity through military escalation). The graduated compliance logic itself affects directionality: proportional enforcement implies that d values oscillate with compliance history. Iranian enrichment increases raise Iran's d (increasing experienced extraction); proportional sanctions increases lower d back (decreasing extraction). This oscillating d structure is unusual — most constraints have stable directionality, but this one is designed to fluctuate with compliance trajectory.
 *
 * MANDATROPHY ANALYSIS:
 *   The JCPOA constraint resolves mandatrophy by distributing classification across perspectives according to structural position rather than forcing a single type. Pragmatic diplomacy sees Rope (pure coordination); Iran sees Tangled Rope (mixed); strict non-proliferation doctrine sees Snare (pure extraction); regional security sees Tangled Rope (mixed); IAEA sees Scaffold (temporary coordination with sunset); NPT authority sees Piton (degraded ritual). The constraint is NOT all six types — the mandatrophy is resolved by recognizing that different agents genuinely have different structural experiences. The potential mandatrophy emerges from the false summit (Mountain) perspective — if one claims the constraint is a natural law of treaty reciprocity, one naturalizes what is actually a contingent institutional arrangement with significant structural contestation. This reading avoids that trap by locating the constraint in the middle typological space (Tangled Rope at the organizational/diplomatic level, Snare at the doctrine level, Rope at the pragmatic level) where partial coordination and partial extraction coexist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calibration_indeterminacy,
    'What metric defines ''proportional'' enrichment increase to ''proportional'' sanctions relief? (Uranium enrichment level? Centrifuge installation rate? Scope of monitoring? Timeline of verification?) Is proportionality objective or negotiated?',
    'Comparative analysis of three scenarios: (A) Iran enriches to 20% while violating JCPOA snapback triggers — how is remediation calibrated? (B) Historical precedent in prior arms control agreements (START, INF) — what metrics were used for proportional response? (C) Formal proportionality doctrine in international law — does a stable principle exist or is every case adjudicated separately?',
    'If proportionality is objective: the constraint is a Rope (coordination with shared metric). If negotiated ad hoc: the constraint is a Snare (extraction disguised as proportional response). The JCPOA''s design allows both — creating structural ambiguity about whether violations will trigger calibrated response or serve as pretext for escalation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_calibration_indeterminacy, conceptual, 'Proportionality metric indeterminacy — how is ''proportional'' calibrated?').

omega_variable(
    verification_asymmetry_tolerance,
    'Does the IAEA verification regime (Complementary Access, Additional Protocol, past military dimensions investigation) represent true reciprocal transparency or extractive intelligence asymmetry favoring non-Iranian parties?',
    'Comparison of JCPOA sampling access scope with prior arms control agreements (USSR/US INF verification, South Africa nuclear disclosure, Libya nuclear disclosure). Analysis of information flows: what data does Iran receive about verification process vs data the P5+1 receive about Iran?',
    'If reciprocal: graduated compliance logic is structurally sound (both sides verify proportionality). If asymmetric: verification itself is a hidden extraction mechanism — the graduated compliance framework provides cover for intelligence collection disproportionately favoring the P5+1.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_tolerance, empirical, 'Whether IAEA verification scope is reciprocal or asymmetric').

omega_variable(
    binding_vs_provisional_status,
    'Is the JCPOA a binding treaty under international law that survives state succession and domestic political transitions, or a provisional accord that can be revoked by unilateral political change (as the Trump administration asserted)?',
    'Comparative jurisprudence on treaty bindingness: Vienna Convention on Law of Treaties, ICJ precedent on treaty withdrawal (Namibia case, Nicaragua case), state practice on successor government treaty obligations. Does JCPOA''s absence from formal Senate ratification (in US context) imply provisional status?',
    'If binding: the constraint is a Mountain or Tangled Rope — immutable once entered. If provisional: the constraint is a Scaffold — the sunset is forced rather than designed. This reading instantiates a middle position: the constraint is binding as a matter of international practice and reciprocal commitment, but the binding mechanism is behavioral/reputational rather than absolute legal obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binding_vs_provisional_status, conceptual, 'Treaty bindingness status — binding vs. provisional').

omega_variable(
    side_agreement_enforceability,
    'Do the secret side agreements between Iran and the IAEA (the PMD [past military dimensions] side letters) have the same legal weight as the main JCPOA text? Can verification findings from secret agreements trigger proportional sanctions changes?',
    'Analysis of JCPOA side letter disclosure and enforcement logic. Comparison with other treaties involving confidential compliance protocols. Examination of whether sanctions triggers cite PMD findings or only open JCPOA terms.',
    'If side agreements are enforced equally: the constraint is Tangled Rope with full transparency. If secret agreements have lower weight or are selectively disclosed: the constraint is Snare for non-signatory parties (who bear consequences of secret compliance findings they cannot observe).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(side_agreement_enforceability, empirical, 'Secret side agreements enforceability and binding weight').

omega_variable(
    reading_kernel_ambiguity,
    'What is the contested kernel this reading instantiates? Is it ''the JCPOA as such'' or ''treaty bindingness in the nuclear context''? Do the sibling readings (binding_multilateral, transactional_provisional) agree on what the kernel IS?',
    'Textual analysis of JCPOA preamble and operative language. Examination of negotiating history — what did parties claim they were creating? Comparison with sibling reading authority grounds and axioms.',
    'If readings differ on kernel identification: the constraint is structurally fragile (parties are bound to different documents). If they agree on kernel but differ on how it binds: the constraint is robust but flexible (same commitment, different readings). This omega documents the foundational committer-level indeterminacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Kernel identity indeterminacy — what document/commitment is being read?').

omega_variable(
    snapback_trigger_discretion,
    'The JCPOA snapback mechanism (reimposition of sanctions if violations cross thresholds) is described as ''automatic'' but actually requires consensus non-renewal of sanctions waivers. Does this discretionary element convert automatic snapback into negotiated response?',
    'Procedural analysis of JCPOA snapback language (JCPoA Annex V) vs actual UN Security Council practice. Historical cases: (A) Did P5 states treat snapback as automatic when Iran tested missiles in 2015-2020? (B) What threshold violations occurred that did NOT trigger snapback? (C) How many days elapsed between violation detection and snapback decision?',
    'If snapback is truly automatic: graduated compliance framework is robust. If discretionary: snapback is a political choice, not mechanical response — the constraint becomes a Piton (theater of automatic enforcement) rather than true Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(snapback_trigger_discretion, empirical, 'Snapback mechanism discretion — automatic or negotiated?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_grad_theater_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jcpoa_grad_theater_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.58).
narrative_ontology:measurement(jcpoa_grad_theater_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.58).

% Extraction over time
narrative_ontology:measurement(jcpoa_grad_extract_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jcpoa_grad_extract_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(jcpoa_grad_extract_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_grad_supp_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(jcpoa_grad_supp_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(jcpoa_grad_supp_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_uranium_enrichment_technical_capacity).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, snapback_sanctions_trigger_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, npt_zero_enrichment_norm).

% DUAL FORMULATION NOTE:
% This story is part of a constraint family decomposed from the natural-language concept 'JCPOA bindingness.' Three structurally distinct readings produce different epsilon values and classification types: (1) binding_multilateral (ε ≈ 0.25, Mountain) — treats JCPOA as universal law; (2) transactional_provisional (ε ≈ 0.62, Snare) — treats JCPOA as contingent deal; (3) graduated_compliance (ε = 0.52, Tangled Rope, this story) — treats JCPOA as reciprocal commitment with proportional enforcement. The readings coexist at the international level (different parties hold different readings) and are connected by reading_relations (coexist_with edges). Each reading's beneficiary/victim structure and authority grounding differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, institutional, 0.28).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
