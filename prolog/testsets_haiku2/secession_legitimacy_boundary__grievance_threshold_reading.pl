% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy via Grievance Threshold (Constitutional Override Reading)
 *   domain: political_economy/federalism/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the grievance-threshold reading of the
 *   secession legitimacy kernel: unilateral secession becomes
 *   constitutionally legitimate when federal actions demonstrably cross a
 *   threshold of structural injustice, overriding constitutional text. The
 *   reading depends on objective burden-of-proof: federal overreach must be
 *   established through external evidence (resource extraction patterns,
 *   fiscal asymmetry, political exclusion, cultural suppression), not merely
 *   claimed. The victim set (federal authority, subordinate populations
 *   opposing secession, constitutional text adherents) exists ONLY if the
 *   threshold is crossed. The secessionist movement benefits from the
 *   reading's framework by gaining legitimacy beyond constitutional
 *   amendment; international arbiters benefit by gaining a standard for
 *   recognition that substitutes their judgment for unilateral determination.
 *   This reading coexists with but competes structurally against three
 *   siblings: constitutional_impossibility_reading (secession never
 *   legitimate unilaterally), popular_sovereignty_reading (regional
 *   referendum is self-legitimating regardless of federal injustice), and
 *   treaty_primacy_reading (indigenous treaty rights are the sole legitimacy
 *   source). The claim/metric gap is deliberate: measured extractiveness and
 *   suppression are high because the reading's operation requires active
 *   enforcement (federal must defend against threshold claims, international
 *   arbiters must police the burden-of-proof), yet the reading's own framing
 *   emphasizes conditional legitimacy (threshold legitimacy is NOT pure
 *   extraction but rather justified override). Do not reconcile the claim to
 *   the metrics; they are independent authored facts.
 *
 * KEY AGENTS:
 *   - regional_secessionist_movement: principal beneficiary (agenda_setter, organized/mobile exit, generational horizon) — gains legitimacy and exit optionality from threshold doctrine
 *   - federal_authority: primary payer (institutional, constrained exit, civilizational horizon) — bears cost of territorial fragmentation risk and institutional erosion
 *   - subordinate_populations_opposing_secession: trapped payers (powerless, identity_locked exit at regional level, biographical horizon) — bear secession costs despite opposition; consent not required
 *   - international_arbiters: secondary beneficiaries (institutional, arbitrage exit, generational horizon) — gain leverage and recognition authority via threshold framework
 *   - constitutional_text_adherents: identity_locked payers (moderate/institutional power split, civilizational horizon) — framework legitimacy authority displaced by external injustice standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy via Grievance Threshold (Constitutional Override Reading)").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/constitutional_law").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, '304259d1-a777-4ce7-a527-2b0fdfc02d03').
narrative_ontology:cs_kernel_codification('304259d1-a777-4ce7-a527-2b0fdfc02d03', fixed_text).
narrative_ontology:cs_authority_grounding('304259d1-a777-4ce7-a527-2b0fdfc02d03', extraction).
narrative_ontology:cs_interpretation_layer_present('304259d1-a777-4ce7-a527-2b0fdfc02d03').
narrative_ontology:cs_reading_relation('304259d1-a777-4ce7-a527-2b0fdfc02d03', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('304259d1-a777-4ce7-a527-2b0fdfc02d03', secession_legitimacy_boundary__popular_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('304259d1-a777-4ce7-a527-2b0fdfc02d03', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('304259d1-a777-4ce7-a527-2b0fdfc02d03', foundational, structural_injustice_overrides_constitutional_text).
narrative_ontology:cs_axiom_status(structural_injustice_overrides_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('304259d1-a777-4ce7-a527-2b0fdfc02d03', structural_injustice_overrides_constitutional_text, deontological).
narrative_ontology:cs_axiom('304259d1-a777-4ce7-a527-2b0fdfc02d03', foundational, threshold_legitimacy_requires_objective_burden_of_proof).
narrative_ontology:cs_axiom_status(threshold_legitimacy_requires_objective_burden_of_proof, holdable).
narrative_ontology:cs_axiom_grounding('304259d1-a777-4ce7-a527-2b0fdfc02d03', threshold_legitimacy_requires_objective_burden_of_proof, empirically_contingent).
narrative_ontology:cs_axiom('304259d1-a777-4ce7-a527-2b0fdfc02d03', secondary, federal_authority_derives_legitimacy_from_non_extraction).
narrative_ontology:cs_axiom_status(federal_authority_derives_legitimacy_from_non_extraction, holdable).
narrative_ontology:cs_axiom_grounding('304259d1-a777-4ce7-a527-2b0fdfc02d03', federal_authority_derives_legitimacy_from_non_extraction, instrumental).
narrative_ontology:cs_reference_frame('304259d1-a777-4ce7-a527-2b0fdfc02d03', constitutional_federal_legitimacy).
narrative_ontology:cs_drift_state('304259d1-a777-4ce7-a527-2b0fdfc02d03', contemporary_secessionist_mobilization_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('304259d1-a777-4ce7-a527-2b0fdfc02d03', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, regional_secessionist_movements).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority_structure).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, subordinate_populations_opposing_secession).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, international_arbiters).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_text_adherents).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, other_federal_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, international_stability_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims federal actions have crossed a threshold of structural injustice (resource extraction, political exclusion, cultural suppression, fiscal asymmetry). Asserts this crossing legitimizes unilateral secession regardless of constitutional text. Mobilizes internal and international political support. Frames the legitimacy claim as conditional on demonstrable federal overreach meeting an objective standard, not on desire to exit alone.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, regional_secessionist_movement, agenda_setter,
    organized, generational, mobile, regional).

% Bears the cost of the constraint's operation: loss of territorial control, erosion of constitutional authority, institutional fragmentation. Faces pressure to defend its legitimacy against threshold-crossing claims. Cannot exit the constraint itself without conceding the threshold doctrine. Must actively enforce the boundary of permissible secession claims or lose territorial integrity.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_authority, payer,
    institutional, civilizational, constrained, national).

% Residents of the secessionist region who oppose exit (minorities, economic dependents on federal transfer, those with identity-level commitment to the larger federation). Bear the cost of secession decision made by regional majority. Their consent is not required by the grievance-threshold reading; threshold legitimacy overrides their preference to remain.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, subordinate_populations_opposing_secession, payer,
    powerless, biographical, trapped, regional).

% International bodies (UN, regional organizations, major powers) benefit from the threshold doctrine by gaining a framework for recognizing secessions that claim federal injustice. The framework substitutes their judgment (did the threshold actually cross?) for unilateral determination. Creates opportunities for diplomatic leverage, recognition withholding, and alignment with secession movements.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_arbiters, beneficiary,
    institutional, generational, arbitrage, global).

% Must interpret what counts as a threshold-crossing injustice. Their rulings determine the constraint's operative definition. They set the evidentiary burden, the criteria for 'structural' versus 'temporary' federal excess, and whether particular grievances qualify. They simultaneously serve as neutral arbiters and as agents defending federal institutional legitimacy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_judiciary_and_constitutional_interpreters, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__grievance_threshold_reading, federal_judiciary_and_constitutional_interpreters, observer).

% Persons and institutions whose legitimacy commitments rest on constitutional text as supreme law. The threshold reading directly displaces their framework by authorizing constitutional override on grounds external to the text. They bear the cost of institutional authority erosion and the collapse of their reference frame for legitimate state action.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, constitutional_text_adherents, payer,
    moderate, civilizational, identity_locked, national).

% Regions that might also claim threshold-crossing (resource-producing regions extracting federal redistribution, cultural minorities, economically marginal zones). The constraint expands their exit options and creates competitive incentives for secession claims. They pay the cost of territorial fragmentation and institutional uncertainty but gain leverage over federal concessions.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, other_federal_regions, payer,
    organized, generational, mobile, national).

% Groups experiencing injustice that predates or is orthogonal to federal structure (slavery descendants, indigenous peoples, formerly colonized populations). The threshold reading focuses on federal overreach, not historical injustice per se. Their voices are excluded from the mechanism even when their historical grievances might meet the threshold test.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, historical_injustice_claimants, excluded,
    powerless, civilizational, trapped, regional).

% States and institutions whose interests rest on territorial stability and predictable borders. The threshold reading destabilizes the Westphalian order by making territorial integrity conditional on federal conduct. They bear externalities of increased secession claims, border disputes, and institutional fragmentation.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_stability_interests, payer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__grievance_threshold_reading, regional_secessionist_movement).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__grievance_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Determines legitimacy boundary for provincial/regional exit from federal authority by establishing objective burden-of-proof standard (structural injustice threshold). Solves coordination problem: when is unilateral exit justified? Answer: when federal crosses threshold. Provides framework for distinguishing just exit from preference-driven secession.
% TRANSFER_FUNCTION: Transfers authority over exit legitimacy from federal institutions and constitutional text to secessionist movements (who can claim threshold-crossing) and international arbiters (who validate the claim). Transfers territorial control from federal to secessionist region if threshold is crossed. Transfers recognition authority from state-of-origin to international consensus on whether injustice threshold met.
% ABSENT_VOICES: Subordinate populations within the secessionist region who oppose exit are excluded: the reading's legitimacy standard (federal structural injustice) overrides local referendum. Constitutional text adherents are present as stakeholders but their framework is displaced by external standard. Indigenous peoples are excluded unless their treaty-based grievances fit the federal-overreach frame. International stability interests (status-quo oriented states) are present as observers but have no veto on secessionist movements meeting threshold.
% DISAPPEARANCE_RATIONALE: If this reading vanished, federal authority over territorial integrity would stabilize (constitutional text and unilateral prohibition would reassert). Secessionist movements would revert to constitutional amendment pathway (orders of magnitude slower and harder to achieve). International recognition would follow strict state-capacity / great-power-consent criteria rather than injustice-threshold criteria. Territorial fragmentation risk would drop substantially. Institutional legitimacy of constitutional text would recover. Regional populations retaining veto via referendum (popular_sovereignty_reading would become default frame for remaining-in-union populations, not federal prohibition). Overall: world rearranges away from secessionist advantage and toward federal-institutional stability.
% FOUNDING_PROBLEM: Constitutions become tools of systematic extraction: territorial resource monopolies, fiscal asymmetries that drain peripheral regions, political exclusion of regional majorities from federal decision-making, suppression of regional cultural identity. Constitutional amendment process is too rigid (requires supermajorities, can be indefinitely blocked). Exit via negotiation is asymmetrically constrained (federal holds veto and has no incentive to grant exit). Problem: how can regions access legitimate exit when federal authority has become extractive but still claims constitutional legitimacy?
% FOUNDING_PROBLEM_CORROBORATION: Historical secessionist movements (Quebec 1960s-1995, Catalonia 2010s, South Sudan pre-2011) cite federal resource extraction and political exclusion as primary grievances; their claims corroborated by economic historians (resource drain calculations), political scientists (representation asymmetry data), and international observers who recognized South Sudan on justice grounds. Federal authorities contest that founding problems exist, arguing constitutional amendment process remains adequate. Neutral corroboration comes from academic analysis of federal asymmetries and from international legal scholars who distinguish legitimate from opportunistic secessions; no consensus, but substantial body of work from non-benefiting parties supports the problem exists.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__grievance_threshold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.68 at interval end, reflecting that the reading's operation transfers authority over exit legitimacy from constitutional text to external burden-of-proof and international recognition. This transfer constitutes extraction from those whose legitimacy rested on the text (constitutional adherents) and those who benefit from territorial stability (subordinate populations opposing secession, international stability interests). The measured trajectory rises from 0.42 to 0.68, indicating that as the reading gains acceptance in international discourse, the extractive cost (institutional authority erosion, border destabilization) accumulates. Suppression measures 0.71 at interval end because the constraint requires active enforcement: federal must construct and defend arguments that claimed injustices do NOT cross the threshold; secessionist movements must marshal evidence that they DO; international arbiters must establish and police evidentiary standards. This is not coercive suppression in the physical sense but rather epistemic enforcement — the regime of evidence and argument that determines the binding threshold. Theater_ratio of 0.42 reflects moderate performative activity: the threshold doctrine legitimizes real institutional conflict (secessionist movements do gain leverage from it), but a share of enforcement effort goes to theatrics (staging 'objective' burden-of-proof hearings whose outcomes track political power rather than evidence, performative international arbitration that respects great-power interests in territorial stability). The measurement series track rising extractiveness and suppression from t=0 to t=15, then plateau (observed data through t=15, projected from t=20 onward), indicating that the constraint stabilizes once international norms around threshold-legitimacy settle. Accessibility_collapse and resistance show complementary dynamics: at organizational and class levels (secessionist movements, regional populations) alternatives collapse as the threshold reading spreads (accessibility rises, resistance persists). At structural and individual levels, accessibility remains partial because federal constitutional process remains available (alternative frame: constitutional_impossibility_reading) and individual actors retain exit options via migration.
 *
 * PERSPECTIVAL GAP:
 *   From the secessionist movement's seat, the reading is coordination: it solves the problem of unjust federal authority by providing a legitimate exit path that respects democratic self-determination. From the federal authority's seat, the reading is pure extraction: it destabilizes territorial integrity without providing federal any ability to stabilize the boundary (the only way federal avoids threshold-crossing claims is to become MORE just, an asymmetric burden). From subordinate populations' seat, the reading is suppression: their consent is overridden by majority-level threshold determination. From constitutional adherents' seat, the reading is authority erosion: it breaks the rule of law by allowing external standard (injustice) to override internal text (constitutional prohibition). From international arbiters' seat, the reading is coordination (provides a standard for recognition) and extraction (allows them to leverage recognition for geopolitical gain). These divergent perceptions are STRUCTURAL, not observational: they follow from the reading's actual mechanics, where threshold legitimacy transfers authority between seats with conflicting interests. The engine computes these differences per-seat from the structural data (beneficiaries/victims, power atoms, exit options, stakeholder roles). The claimed_type (tangled_rope) indicates that both coordination function and asymmetric extraction are present: the coordination is threshold-determination (legitimate vs illegitimate exit), the extraction is that determination privileges secessionist movements and undermines federal authority. This is textbook tangled rope: genuine coordination function wrapped around asymmetric benefit distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   The regional_secessionist_movement sits near the full-beneficiary end (d near 0.0): the reading directly authorizes their exit, gives them a legitimacy frame independent of federal consent, and amplifies their leverage in negotiation. Their exit options are mobile (geographic exit, international recognition pathways), power is organized but subordinate to federal institutional capacity, and the reading directly benefits their structural position. Federal_authority sits near the full-target end (d near 1.0): the reading extracts institutional legitimacy and territorial control, constrains federal power to enforce constitutional unity, and forces federal to defend against continuous threshold claims. Federal exit options are structurally constrained (cannot dissolve federal authority without conceding the reading's premise). Subordinate_populations_opposing_secession sit high on the extraction axis (d near 0.9) despite nominal powerlessness: they bear secession exit costs (economic disruption, loss of federal safety nets, forced political identity shift) and have NO veto power (the reading overrides local referendum). Their exit is trapped (cannot exit the region easily, cannot prevent secession if threshold meets standard). Constitutional_text_adherents have moderate d (near 0.6): they bear institutional authority erosion but retain their textual framework as an alternative reading (the reading does not foreclose constitutional_impossibility_reading, only marginalizes it in practice). International_arbiters have low d (near 0.2): they gain leverage and authority, suffer minimal cost, have arbitrage exit options (can choose which threshold claims to recognize based on geopolitical alignment). The engine derives these directionalities from the beneficiary/victim declarations and exit-option modulation; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is a central issue for this reading. The founding problem ('how can regions exit when federal overreach has become systematic but claims constitutional legitimacy?') is CONTESTED in status (t=15 observations show some federal actions meet threshold criteria, others do not; status remains live for secessionist movements, dead for federal authorities and constitutional adherents). The disappearance verdict is WORLD_REARRANGES (if the threshold reading vanished, federal authority would stabilize, secessionist movements would lose exit legitimacy, international recognition standards would revert to capacity/consent criteria). This mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) activates the mandatrophy detection: the reading persists because multiple parties benefit from it (secessionist movements gain exit optionality, international arbiters gain recognition leverage) DESPITE contested evidence that federal structural injustice is actually occurring. The constraint is not mandatrophy-RESOLVED but rather PRONE TO mandatrophy: it will become mandatrophy if and only if the threshold determination becomes decoupled from evidence (e.g., international arbiters grant recognition based on geopolitical alignment rather than evidence of crossing). The theater_ratio of 0.42 reflects this risk: moderate performative activity suggests the evidentiary burden may already be contaminated by power considerations. The measurement trajectory's plateau at t=15 suggests the reading has reached equilibrium where it functions as organizational-level negotiation leverage rather than as a genuine injustice-detection mechanism. Do NOT author mandatrophy_resolved=true; the reading's mandate remains contested and live (it still functions as a legitimacy claim with real institutional effects), but the corpus should track whether the evidentiary regime degrades into pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_determinacy,
    'What constitutes crossing the threshold of structural injustice? Can the threshold be operationalized with sufficient precision to distinguish legitimate from illegitimate secession claims?',
    'International jurisprudence on secession (EU, AU, UN bodies) attempting to apply the threshold to specific cases (Catalonia, South Sudan, Scotland, Quebec). Evidence of consistent application across cases (threshold is determinate) vs. application tracking geopolitical alignment (threshold is indeterminate).',
    'If threshold is determinate, the reading functions as genuine rule-of-law boundary (tangled rope: coordination + asymmetric enforcement). If indeterminate, the reading becomes pure power-play (snare: legitimacy for favored movements, denial for others), and mandatrophy becomes imminent (the founding problem — how to legitimize just exit — remains officially addressed but functionally displaced by political alignment).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_determinacy, empirical, 'Whether the structural injustice threshold can be operationalized consistently or whether it will become power-indexed.').

omega_variable(
    burden_of_proof_location,
    'Where does the burden of proof rest: on the secessionist movement to prove federal structural injustice, or on federal to prove the claimed injustice does not meet threshold?',
    'International arbitration decisions, constitutional court rulings, diplomatic practice in recognition cases. Consistent requirement that secessionist movements prove threshold-crossing (burden on plaintiff) vs. requirement that federal disprove it (burden on defendant).',
    'Burden placement determines effective directionality. If burden rests on secessionist movements, federal retains institutional advantage and threshold legitimacy is constrained (maintains rope character, lower effective extraction). If burden shifts to federal, secessionist movements gain leverage and threshold legitimacy expands (increases effective extraction, pushes toward snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(burden_of_proof_location, empirical, 'Who bears the burden of proof in threshold determination affects the constraint''s operative character.').

omega_variable(
    threshold_vs_popular_sovereignty,
    'Does the threshold reading foreclose the popular sovereignty reading, or can both coexist? If federal actions DO cross the threshold, does the grievance-threshold reading require that secession also meet a regional referendum threshold?',
    'Cases where threshold is met but regional population opposes secession (or vice versa: threshold not met but referendum approves). Do international arbiters recognize the secession (threshold overrides referendum) or deny it (referendum is also required)? Jurisprudence from Quebec, Catalonia, Kurdistan referenda.',
    'If threshold FORECLOSED popular sovereignty (grievance alone is sufficient to override referendum), the reading is more extractive from subordinate populations (they lose consent requirement). If threshold and sovereignty COEXIST (both required), directionality is modulated (subordinate populations retain veto via referendum, secessionist movements need two-part proof).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_vs_popular_sovereignty, conceptual, 'Whether threshold legitimacy absorbs or coexists with referendum legitimacy affects subordinate populations'' extraction risk.').

omega_variable(
    treaty_primacy_interaction,
    'Where do indigenous treaty rights sit relative to the threshold: do they constitute grounds for recognizing threshold-crossing (indigenous-specific injustice meets threshold), or is treaty primacy a separate orthogonal legitimacy channel?',
    'Indigenous peoples'' positions in secession disputes where both treaty and threshold claims are live (e.g., indigenous nations within proposed secessionist regions). Do international arbiters apply threshold test to indigenous grievances (treaty converted to federal structural injustice), or do they apply treaty_primacy_reading as distinct framework?',
    'If treaty claims feed threshold analysis, indigenous peoples gain leverage via threshold mechanism. If treaty is orthogonal, indigenous peoples can be excluded from secessionist arrangements that meet threshold-crossing but violate treaty. The victim set definition changes: if orthogonal, indigenous peoples become newly invisible victims; if integrated, they become beneficiaries via grievance pathway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_primacy_interaction, conceptual, 'Whether indigenous treaty rights integrate into or remain orthogonal to threshold legitimacy affects indigenous agency in secession cases.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.71) structural (external barriers to threshold challenges: legal barriers, diplomatic non-recognition, institutional gatekeeping) or internalized (secessionist movements self-censor threshold claims, federal internalizes threshold as constraint on action)?',
    'Post-implementation behavioral tracking: if threshold mechanism is removed, do secessionist movements continue to self-constrain their claims (internalized), or do they immediately escalate (structural suppression was primary restraint)? How do federal decision-makers behave once threshold is salient: do they actively police their conduct to avoid threshold-crossing (internalized constraint), or do they continue prior conduct patterns (threshold is external noise)?',
    'If internalized, the suppression is sustainable (constraints persist even when enforcement apparatus is weakened). If structural, suppression persists only under active enforcement (mechanism is brittle). This affects the constraint''s long-term stability and whether the reading becomes piton-like (performative maintenance required).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized affects constraint''s sustainability and mandatrophy risk.').

omega_variable(
    reading_foreclosure_by_constitutional_amendment,
    'Does this reading foreclose constitutional_impossibility_reading, or merely marginalize it? If a federal constitution is amended to recognize conditional secession rights (threshold-based), does the constitutional_impossibility_reading become OVERRIDDEN within the same framework?',
    'Constitutional amendment cases where threshold doctrine is formally codified (Spain, Canada). Does formal codification eliminate the logical possibility of absolute constitutional prohibition, or do constitutional originalists maintain that the amendment is itself illegitimate?',
    'If amendment can FORECLOSE the impossibility reading, directionality shifts: the grievance_threshold becomes the new constitutional text (beneficiaries of the new text include secessionist movements, losers include absolute prohibitionists). If amendment cannot foreclose (originalists maintain legitimacy of the old text), the readings remain in perpetual coexistence and the constraint operates as a standing dispute rather than a settled rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_by_constitutional_amendment, conceptual, 'Whether constitutional amendment can foreclose impossibility reading affects the long-term structural relationship between these readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(sece_grid_01, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(class), 0, 0.62).
narrative_ontology:measurement(sece_grid_02, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(class), 40, 0.68).
narrative_ontology:measurement(sece_grid_03, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(individual), 0, 0.35).
narrative_ontology:measurement(sece_grid_04, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(individual), 40, 0.52).
narrative_ontology:measurement(sece_grid_05, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(organizational), 0, 0.58).
narrative_ontology:measurement(sece_grid_06, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(organizational), 40, 0.72).
narrative_ontology:measurement(sece_grid_07, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(sece_grid_08, secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse(structural), 40, 0.64).
narrative_ontology:measurement(sece_grid_09, secession_legitimacy_boundary__grievance_threshold_reading, resistance(class), 0, 0.68).
narrative_ontology:measurement(sece_grid_10, secession_legitimacy_boundary__grievance_threshold_reading, resistance(class), 40, 0.76).
narrative_ontology:measurement(sece_grid_11, secession_legitimacy_boundary__grievance_threshold_reading, resistance(individual), 0, 0.52).
narrative_ontology:measurement(sece_grid_12, secession_legitimacy_boundary__grievance_threshold_reading, resistance(individual), 40, 0.68).
narrative_ontology:measurement(sece_grid_13, secession_legitimacy_boundary__grievance_threshold_reading, resistance(organizational), 0, 0.71).
narrative_ontology:measurement(sece_grid_14, secession_legitimacy_boundary__grievance_threshold_reading, resistance(organizational), 40, 0.74).
narrative_ontology:measurement(sece_grid_15, secession_legitimacy_boundary__grievance_threshold_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(sece_grid_16, secession_legitimacy_boundary__grievance_threshold_reading, resistance(structural), 40, 0.73).
narrative_ontology:measurement(sece_grid_17, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(class), 0, 0.48).
narrative_ontology:measurement(sece_grid_18, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(class), 40, 0.72).
narrative_ontology:measurement(sece_grid_19, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(individual), 0, 0.41).
narrative_ontology:measurement(sece_grid_20, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(individual), 40, 0.65).
narrative_ontology:measurement(sece_grid_21, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(sece_grid_22, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(organizational), 40, 0.78).
narrative_ontology:measurement(sece_grid_23, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(sece_grid_24, secession_legitimacy_boundary__grievance_threshold_reading, stakes_inflation(structural), 40, 0.71).
narrative_ontology:measurement(sece_grid_25, secession_legitimacy_boundary__grievance_threshold_reading, suppression(class), 0, 0.44).
narrative_ontology:measurement(sece_grid_26, secession_legitimacy_boundary__grievance_threshold_reading, suppression(class), 40, 0.71).
narrative_ontology:measurement(sece_grid_27, secession_legitimacy_boundary__grievance_threshold_reading, suppression(individual), 0, 0.38).
narrative_ontology:measurement(sece_grid_28, secession_legitimacy_boundary__grievance_threshold_reading, suppression(individual), 40, 0.64).
narrative_ontology:measurement(sece_grid_29, secession_legitimacy_boundary__grievance_threshold_reading, suppression(organizational), 0, 0.52).
narrative_ontology:measurement(sece_grid_30, secession_legitimacy_boundary__grievance_threshold_reading, suppression(organizational), 40, 0.76).
narrative_ontology:measurement(sece_grid_31, secession_legitimacy_boundary__grievance_threshold_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(sece_grid_32, secession_legitimacy_boundary__grievance_threshold_reading, suppression(structural), 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__grievance_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__grievance_threshold_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The secession_legitimacy_boundary kernel decomposes into four structurally distinct constraints, each a different reading. The grievance_threshold_reading (this story) holds that legitimacy is conditional on demonstrable federal structural injustice. Each sibling reading changes the referent: constitutional_impossibility_reading asserts secession is NEVER legitimate unilaterally (epsilon ≈ 0.1, mountain-like, applies to same referent but with different reading); popular_sovereignty_reading asserts legitimacy follows from regional referendum (epsilon varies by region's preference divergence from federal, orthogonal referent); treaty_primacy_reading asserts only indigenous treaty holders determine legitimacy (epsilon depends on treaty language, third referent). These are not the same constraint viewed from different angles — their epsilon values differ structurally because they have different referents for what legitimacy IS. Link via network.affects_constraints to enable contamination analysis and reading family tracking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__grievance_threshold_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
