% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Post-1945 Normative Prohibition on Total War (Article 2(4) and Humanitarian Law) — Normative Reading
 *   domain: international_relations/strategic_studies/commitment_system
 *
 * SUMMARY:
 *   Since 1945, total war — the mobilization of entire societies for the
 *   annihilation of enemy states and populations — has remained physically
 *   buildable but has been placed outside legitimate statecraft by the UN
 *   Charter's Article 2(4) per-se prohibition on aggressive force and by the
 *   humanitarian-law regime built alongside it (Geneva Conventions,
 *   Additional Protocols, the Nuremberg doctrine that aggressive war is the
 *   supreme international crime). This story authors THAT normative
 *   arrangement as one reading of the contested kernel of post-1945 total-war
 *   winnability: the arrangement solves a genuine collective-action problem
 *   (recurrent industrial war), protects civilian populations, and demands
 *   episodic enforcement that its five administering powers gate behind a
 *   veto. KEY AGENTS (by structural relationship):
 *   security_council_permanent_members — agenda-setter
 *   (institutional/arbitrage), administers enforcement and collects the
 *   discretion premium; global_civilian_populations — primary beneficiary
 *   (powerless/trapped); small_status_quo_states — beneficiary
 *   (organized/trapped); revisionist_powers — primary payer
 *   (powerful/constrained), denied the war option; compliant_middle_powers —
 *   payer-beneficiary (moderate/constrained); humanitarian_law_institutions —
 *   beneficiary-custodian (organized/identity_locked); nonstate_armed_groups
 *   — excluded (moderate/trapped); strategic_studies_analysts — analytical
 *   observer.
 *
 * KEY AGENTS:
 *   - security_council_permanent_members: agenda-setter (institutional/arbitrage) — gates enforcement behind the veto, immune from the machinery it runs, collects the discretion premium
 *   - global_civilian_populations: primary beneficiary (powerless/trapped) — protected by the prohibition's delegitimating and deterrent effect
 *   - small_status_quo_states: beneficiary (organized/trapped) — existence secured by collective condemnation they cannot enforce alone
 *   - revisionist_powers: primary payer (powerful/constrained) — denied the war option their predecessors used; bear sanctions and isolation when testing the norm
 *   - compliant_middle_powers: payer-beneficiary (moderate/constrained) — fund law-of-war compliance, receive the protections they finance
 *   - humanitarian_law_institutions: beneficiary-custodian (organized/identity_locked) — ICRC and allied bodies hold mandate and identity from the regime they serve
 *   - nonstate_armed_groups: excluded (moderate/trapped) — governed at the regime's margins, absent from every drafting table
 *   - strategic_studies_analysts: analytical observer — codes compliance, publishes the attribution record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.34).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.44).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.34).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Post-1945 Normative Prohibition on Total War (Article 2(4) and Humanitarian Law) — Normative Reading").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies/commitment_system").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, 'c5fd0e11-c7bd-4670-90e2-9e44d397e46c').
narrative_ontology:cs_kernel_codification('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', fixed_text).
narrative_ontology:cs_authority_grounding('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', lineage).
narrative_ontology:cs_interpretation_layer_present('c5fd0e11-c7bd-4670-90e2-9e44d397e46c').
narrative_ontology:cs_reading_relation('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_reading_relation('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', total_war_winnability_post1945__strategic_culture_drift, influences).
narrative_ontology:cs_axiom('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', foundational, aggressive_war_illegitimate_per_se).
narrative_ontology:cs_axiom_status(aggressive_war_illegitimate_per_se, holdable).
narrative_ontology:cs_axiom_grounding('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', aggressive_war_illegitimate_per_se, deontological).
narrative_ontology:cs_axiom('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', foundational, positive_law_not_prudence_sources_restraint).
narrative_ontology:cs_axiom_status(positive_law_not_prudence_sources_restraint, holdable).
narrative_ontology:cs_axiom_grounding('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', positive_law_not_prudence_sources_restraint, conventional).
narrative_ontology:cs_reference_frame('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', charter_collective_security_settlement).
narrative_ontology:cs_drift_state('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', contemporary_multipolar_veto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c5fd0e11-c7bd-4670-90e2-9e44d397e46c', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, small_status_quo_states).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_institutions).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, compliant_middle_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, compliant_middle_powers).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, nuremberg_aggressive_war_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, charter_collective_security_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five governments hold veto power over the enforcement machinery created by the 1945 settlement. They authorize or block collective responses to the use of force, decide which violations reach the agenda, and their own military actions are effectively immune from the machinery they administer. They staff and fund much of the treaty system's bureaucracy and can reshape what the rules mean in application; their exit is not departure but discretionary reinterpretation.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% The class of persons who bear war's direct costs. Since 1945 they have lived under a legal order in which cross-border conquest is presumptively criminal and in which belligerents owe reciprocal duties toward civilians and captured enemies. They cannot exit the system, cannot enforce it themselves, and their protection depends on states honoring obligations those states wrote for themselves.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, biographical, trapped, global).

% States without the military depth to resist a determined larger neighbor. The prohibition substitutes collective condemnation and the prospect of balancing coalitions for the national strength they lack. They organize bloc votes and coalition diplomacy to sustain the norm but cannot defend its guarantees alone; leaving the system would expose them immediately to their larger neighbors.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, small_status_quo_states, beneficiary,
    organized, biographical, trapped, regional).

% Governments with outstanding territorial or hierarchical grievances that the post-1945 settlement froze in place. The prohibition denies them the war option their predecessors used, and when they test it they face sanctions, isolation, and occasionally armed counter-coalitions. Sovereignty means they cannot leave the system; their openings are rhetorical contestation, methods below the prohibition's threshold, and building parallel institutions.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers, payer,
    powerful, generational, constrained, continental).

% States that maintain professional militaries under the humanitarian-law regime: they fund law-of-war training, legal review of targeting, and weapons restrictions that raise operating costs, and in exchange their soldiers and citizens fall under the regime's protections. Running unregulated forces instead would forfeit alliances, trade access, and the reciprocity that shields their own personnel.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, compliant_middle_powers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, compliant_middle_powers, beneficiary).

% The ICRC and the cluster of bodies that custody the Geneva system. They monitor compliance, broker humanitarian access, and draft successive protocol generations; their mandate, funding, and institutional identity are constituted by the regime they serve. Departing the regime would dissolve the organization's purpose rather than relieve it of a burden.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_institutions, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, humanitarian_law_institutions, agenda_setter).

% Armed organizations outside statehood that the regime reaches only at its margins — bound by common-article obligations in theory, prosecuted wholesale in practice, and absent from every drafting table that produced the texts governing them. They would contest the asymmetry by which states write the rules and non-states answer under them.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, nonstate_armed_groups, excluded,
    moderate, immediate, trapped, regional).

% Scholars and analysts who track whether restraint since 1945 tracks the legal order, the weapons, or the culture of elites. They publish the attribution record, code compliance cases, and supply the evidentiary base on which any adjudication of this arrangement's workings depends.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, strategic_studies_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, security_council_permanent_members).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts war initiation from a sovereign prerogative into a collectively policed violation: states can plan defense against a known, condemnable aggressor rather than suspect every neighbor; reciprocal humanitarian obligations protect each belligerent's prisoners and civilians; the expectation that aggression triggers balancing coalitions makes conquest unprofitable before it starts.
% TRANSFER_FUNCTION: Moves the war-making option from every state's unilateral discretion into a pooled, Council-gated reserve; moves compliance costs (law-of-war training, weapons restrictions, legal review) onto belligerent militaries; moves enforcement discretion to the five permanent members; moves protection to civilians and persons hors de combat.
% ABSENT_VOICES: Non-state armed groups are regulated at the regime's margins but sat at none of its drafting tables; colonized peoples were absent in 1945 when the territorial settlement was fixed; small states facing a blocked Council have no forum when a veto silences enforcement. Each would object that the settlement's terms were written without them and are applied to them selectively.
% DISAPPEARANCE_RATIONALE: Overnight repeal would return conquest to the lawful option set: mobilization doctrines, territorial claims, and alliance guarantees keyed to the prohibition would unwind within years; balancing would become uncertain again, arms competition would deepen, and the reciprocal protections of humanitarian law would lose their legal anchor. The physical capacity for total war persists — nothing else currently occupies the coordinating role this arrangement fills.
% FOUNDING_PROBLEM: Two industrial world wars within three decades killed on the order of a hundred million people, erased cities, and normalized deliberate starvation and genocide; the interwar renunciation of war failed for want of institutional teeth; the founders sought a binding prohibition backed by enforcement machinery.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration of the founding problem comes from outside the benefiting parties: ICRC wartime archives and neutral-state diplomatic records document the scale and character of the 1939-45 destruction that motivated the drafters, and the Nuremberg judgment articulated the per-se crime in public reasoning. Corroboration of the problem's PRESENT status splits: historical scholarship notes no great-power total war since 1945, while strategic-studies and legal literature disputes whether the machinery now serves impartial coordination or selective administration — no single outside source settles the status, and the split itself is the signal.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is rope from structure: the arrangement solves a real collective-action problem (recurrent industrial war), its participant majority is a net beneficiary, steady-state coercive overhead is low (legitimacy and reciprocity carry daily carriage), and alternatives beyond the prohibited act — limited war, gray-zone coercion, lawfare, economic statecraft — remain open, so accessibility collapse is partial (0.38). Metrics are authored independently as descriptive facts: extractiveness 0.34 (steady-state extraction low; episodic heavy costs land on norm-testers; the permanent members' discretion premium is real but second-order next to the coordination product); suppression 0.44 (coercion is episodic — sanctions, authorized force — not continuous); theater_ratio 0.35 (ritual accumulation in assembly debate and anniversary diplomacy, but the signaling and reciprocity core still functions); resistance 0.55 (open contestation, double-standard critique, episodic violations, parallel institution-building). The suppression_requirement series is deliberately included because this story tracks enforcement-capacity change: it oscillates rather than trends — founding ambition (Korea-era, 0.55), Cold War veto paralysis (0.30-0.35), 1990s revival (Gulf War, ad hoc tribunals, Rome Statute, 0.62), post-Libya erosion (0.44). The cycle is driven by great-power concord and discord — an external political factor, not intermittent reinforcement as an extraction mechanism — and the base_properties scalars reflect the late-erosion phase of the cycle at t=80. All three series share one time grid (every 10 units, 1945-2025) so no metric borrows another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently from identical structural data. From the revisionist-payer seat the arrangement reads as enforced option-denial: the same prohibition that others experience as protection operates on them as a locked door backed by sanctions, and their constrained exit (sovereignty permits no departure from the system) amplifies the experienced burden. From the civilian and small-state beneficiary seats the same structure reads as subsidized security. From the agenda-setter seat it reads as legitimate stewardship layered with a discretion premium — the administrator experiences the veto not as a cost but as the arrangement's principal asset. Coalition potential among payers is weak: revisionist powers are few, geographically dispersed, and mutually suspicious, which is precisely why their aggregate grievance has not converted into effective resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: global_civilian_populations (powerless, trapped) sit nearest the full-beneficiary end — the arrangement subsidizes them at no administrative price; small_status_quo_states likewise, with trapped exit deepening the subsidy reading. humanitarian_law_institutions are identity_locked beneficiaries: the institutional-identity fusion is constitutive — the ICRC has become its custodial function, so exit would dissolve the organization rather than free it; if that identity frame broke, its seat would recompute from subsidized custodian to ordinary interested party. compliant_middle_powers carry a genuine dual position (pay compliance costs, collect protection), landing mid-low. revisionist_powers are the declared victims: powerful but constrained exit places them near the full-target end, and the engine should compute their seat as markedly more extractive than the beneficiary seats experience. security_council_permanent_members derive low directionality from their agenda-setting position, but the discretion premium is flagged in the receipt surface rather than papered over. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, and the arrangement's global scope modestly amplifies effective extraction through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — recurrence of industrial total war — has not materialized among great powers since 1945, yet the machinery has persisted and accreted ritual (commemorative summits, unenforced resolutions, protocol generations ratified faster than they are applied). The R5 interview returns status=contested paired with verdict=world_rearranges: the parties dispute whether the problem is live, but no party claims the world would be unchanged by the arrangement's removal, so the mismatch-only consumer finds no dead-plus-rearranges zombie flag. What the classification prevents: reading the episodic enforcement and the P5 premium as proof of pure extraction would erase the largest coordination product in modern history; conversely, reading the rope claim as settling the matter would miss the rising theater series and the capture question, which are exactly the signals that would date a drift from coordination toward administered ritual if they continue.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the normative_reading_drop reading of kernel total_war_winnability_post1945; what would adopting a sibling reading change structurally, and where is the disagreement located?',
    'Cross-reading comparison on shared cases: if total war''s absence survives holding nuclear reachability constant (structural_contraction_reading''s premise) and holding elite-discourse content constant (strategic_culture_drift''s premise), the normative arrangement retains causal standing; if not, this reading''s epsilon and coordination credit are misattributed.',
    'Adopting structural_contraction_reading would strip the norm of causal credit and shift this constraint toward vacuity or inevitability; adopting strategic_culture_drift would relocate the constraint from treaty text to elite discourse, moving the enforcement surface from Security Council machinery to socialization. The disagreement is located at the causal attribution of the post-1945 great-power peace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings relocate causation of the same disappearance.').

omega_variable(
    p5_enforcement_capture_share,
    'What fraction of the enforcement machinery''s decisions track permanent-member interest rather than impartial violation criteria?',
    'Systematic coding of Chapter VII activation against veto patterns, sponsor identity, and violation severity across the full case population.',
    'A high capture share would recompute the arrangement as hybrid coordination/extraction with the permanent members as the capturing seat; a low share would confirm the coordination-first reading and the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_enforcement_capture_share, empirical, 'Magnitude of great-power discretion premium inside the enforcement machinery.').

omega_variable(
    legitimacy_vs_deterrence_compliance_mix,
    'Within this reading, how much observed restraint comes from internalized illegitimacy versus fear of enforcement or retaliation?',
    'Compare restraint behavior across dyads varying in enforcement reachability while holding capability and grievance profiles fixed; survey and doctrinal-text analysis of elite justifications.',
    'If restraint is mostly fear-driven, the norm is thinner than claimed: suppression does the work, the coordination credit shrinks, and the arrangement sits closer to enforced deterrence than to settled law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_vs_deterrence_compliance_mix, empirical, 'Compliance mix between internalized legitimacy and external deterrence inside the normative reading.').

omega_variable(
    ihl_reciprocity_decay_asymmetric_conflict,
    'Is the humanitarian-law component''s reciprocal-protection function decaying under asymmetric warfare, where belligerents no longer face mirror-image obligations?',
    'Longitudinal compliance coding across symmetric versus asymmetric conflicts since 1945, controlling for violation severity and recording capacity.',
    'Decay would raise the theater share of the humanitarian component and push it toward inertial maintenance; intact reciprocity would support the coordination reading of the Geneva system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ihl_reciprocity_decay_asymmetric_conflict, empirical, 'Whether reciprocal protection, the humanitarian regime''s core mechanism, survives asymmetric conflict.').

omega_variable(
    gray_zone_threshold_drift,
    'Where is the operative line between prohibited total war and permitted limited coercion drifting, and is below-threshold activity hollowing the prohibition while nominally preserving it?',
    'Expert elicitation plus case comparison on threshold placement: annexations, proxy wars, blockade, and cyber coercion coded against contemporaneous doctrinal statements.',
    'Threshold erosion would mean the constraint governs less than its text claims, inflating measured compliance and overstating accessibility collapse of the prohibited option.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gray_zone_threshold_drift, conceptual, 'Drift of the practical boundary between prohibited and permitted force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t10, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(tota_tr_t10, observed).
narrative_ontology:measurement(tota_tr_t20, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(tota_tr_t20, observed).
narrative_ontology:measurement(tota_tr_t30, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(tota_tr_t30, observed).
narrative_ontology:measurement(tota_tr_t40, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(tota_tr_t40, observed).
narrative_ontology:measurement(tota_tr_t50, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(tota_tr_t50, observed).
narrative_ontology:measurement(tota_tr_t60, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(tota_tr_t60, observed).
narrative_ontology:measurement(tota_tr_t70, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 70, 0.33).
narrative_ontology:measurement_basis(tota_tr_t70, observed).
narrative_ontology:measurement(tota_tr_t80, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 80, 0.35).
narrative_ontology:measurement_basis(tota_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t10, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(tota_be_t10, observed).
narrative_ontology:measurement(tota_be_t20, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 20, 0.33).
narrative_ontology:measurement_basis(tota_be_t20, observed).
narrative_ontology:measurement(tota_be_t30, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 30, 0.36).
narrative_ontology:measurement_basis(tota_be_t30, observed).
narrative_ontology:measurement(tota_be_t40, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(tota_be_t40, observed).
narrative_ontology:measurement(tota_be_t50, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 50, 0.26).
narrative_ontology:measurement_basis(tota_be_t50, observed).
narrative_ontology:measurement(tota_be_t60, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 60, 0.31).
narrative_ontology:measurement_basis(tota_be_t60, observed).
narrative_ontology:measurement(tota_be_t70, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 70, 0.36).
narrative_ontology:measurement_basis(tota_be_t70, observed).
narrative_ontology:measurement(tota_be_t80, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 80, 0.34).
narrative_ontology:measurement_basis(tota_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t10, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 10, 0.35).
narrative_ontology:measurement_basis(tota_su_t10, observed).
narrative_ontology:measurement(tota_su_t20, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(tota_su_t20, observed).
narrative_ontology:measurement(tota_su_t30, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 30, 0.32).
narrative_ontology:measurement_basis(tota_su_t30, observed).
narrative_ontology:measurement(tota_su_t40, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 40, 0.33).
narrative_ontology:measurement_basis(tota_su_t40, observed).
narrative_ontology:measurement(tota_su_t50, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(tota_su_t50, observed).
narrative_ontology:measurement(tota_su_t60, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(tota_su_t60, observed).
narrative_ontology:measurement(tota_su_t70, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 70, 0.48).
narrative_ontology:measurement_basis(tota_su_t70, observed).
narrative_ontology:measurement(tota_su_t80, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 80, 0.44).
narrative_ontology:measurement_basis(tota_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'why did total war disappear after 1945' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file is the normative_reading_drop member: it authors epsilon for the standing normative arrangement (prohibition plus humanitarian law) assessed by that reading's own lights. The structural_contraction_reading member authors epsilon for a world in which reachability, not legitimacy, does the work; the strategic_culture_drift member authors epsilon for a discursive constraint whose enforcement surface is socialization rather than treaty machinery. The members are linked pairwise through affects_constraints; neither this file's metrics nor its claim adjudicate the family contest — that is routed to the kernel_reading_indexicality omega.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
