% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Post-Manifesto Doctrine-Practice Gap in Plural Marriage Commitment (1890-1904)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   Between the October 1890 public declaration suspending new plural
 *   marriages and the April 1904 second declaration closing the practice, the
 *   institution maintained a standing gap between its canon and its conduct:
 *   Section 132 remained scripture, formally unrevised and publicly affirmed,
 *   while new plural marriages continued - roughly two hundred or more by
 *   historian estimates - performed in Mormon colonies in northern Mexico and
 *   southern Alberta, on international waters, and in other venues
 *   represented as lying outside United States jurisdiction. Leaders answered
 *   congressional and press inquiries with sworn denials that colony records
 *   later contradicted. The gap was not drift; it was administered -
 *   candidates qualified, ceremonies routed, testimony managed - and it was
 *   load-bearing: it let the institution survive criminalization and obtain
 *   statehood without forcing a decision between legal compliance and
 *   doctrinal continuity. The claim/metric stance is deliberate: claimed_type
 *   states the structure as analyzed (tangled_rope - a genuine coordination
 *   function joined to asymmetric extraction under active enforcement); the
 *   metrics describe observed operation independently, and the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - first_presidency_leadership: Agenda-setter and primary beneficiary (institutional/generational/identity_locked) - issued the public declaration, managed compliance and testimony, tolerated covert continuance, captured the flexibility and survival gains
 *   - - elite_sealing_recipient_families: Beneficiary (organized/generational/arbitrage) - received post-declaration sealings via jurisdictional arbitrage across Mexico, Canada, and offshore venues
 *   - - sealing_performing_apostles: Enforcement administrators (powerful/biographical/identity_locked) - performed the covert marriages; dual-positioned, later bearing disciplinary costs
 *   - - manifesto_objecting_apostles: Payer (powerful/biographical/identity_locked) - objected to the ambiguity in council; censured, marginalized, or driven to resignation
 *   - - general_membership: Primary payer (moderate/biographical/trapped) - absorbed bewilderment and betrayed expectations; exit meant forfeiting community, temple standing, and the salvation framework
 *   - - post_manifesto_plural_wives: Payer (powerless/biographical/trapped) - women sealed during the window under ambiguous legal and doctrinal standing
 *   - - fundamentalist_schism_communities: Payer (organized/generational/mobile) - the gap's ambiguity seeded their eventual separation
 *   - - federal_monitoring_authorities: Observer (institutional/generational/analytical) - Senate committees and prosecutors whose testimony demands exposed the gap
 *   - - anti_polygamy_women_activists: Excluded (moderate/biographical/trapped) - church women opposing plural marriage, never consulted
 *   - - institutional_historians: Analytical observer (analytical/civilizational/analytical) - reconstructed the gap from records the institution denied
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.74).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.62).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.74).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Post-Manifesto Doctrine-Practice Gap in Plural Marriage Commitment (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '6fce18f8-2371-43ab-95e7-16a30b1dbd2e').
narrative_ontology:cs_kernel_codification('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', fixed_text).
narrative_ontology:cs_authority_grounding('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', lineage).
narrative_ontology:cs_interpretation_layer_present('6fce18f8-2371-43ab-95e7-16a30b1dbd2e').
narrative_ontology:cs_reading_relation('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', foundational, canonical_principle_immutable_under_exigency).
narrative_ontology:cs_axiom_status(canonical_principle_immutable_under_exigency, holdable).
narrative_ontology:cs_axiom_grounding('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', canonical_principle_immutable_under_exigency, theological).
narrative_ontology:cs_axiom('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', secondary, dual_track_legitimation_authorized).
narrative_ontology:cs_axiom_status(dual_track_legitimation_authorized, holdable).
narrative_ontology:cs_axiom_grounding('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', dual_track_legitimation_authorized, instrumental).
narrative_ontology:cs_reference_frame('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', principle_immutable_practice_administrable).
narrative_ontology:cs_drift_state('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', post_manifesto_decade, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6fce18f8-2371-43ab-95e7-16a30b1dbd2e', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, first_presidency_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, elite_sealing_recipient_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_plural_wives).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_schism_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, sealing_performing_apostles).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, manifesto_objecting_apostles).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, section_132_canonical_immutability).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__practice_doctrine_gap, prophetic_dual_track_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issued the 1890 public declaration suspending new plural marriages, answered congressional and press inquiries (at times inaccurately), authorized or tolerated continued sealings in jurisdictions represented as outside United States law, and finally issued the 1904 second declaration closing the practice. Gains flowing to this seat: institutional continuity, Utah statehood in 1896, and discretion over timing and framing. Exit from the arrangement was effectively unavailable - the office, its vows, and the community's identity were fused with the arrangement being managed.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, first_presidency_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, first_presidency_leadership, beneficiary).

% Prominent families with outstanding sealing promises received new plural marriages between 1890 and 1904, often performed in the Mormon colonies of northern Mexico or southern Alberta, aboard international waters, or in other venues represented as beyond United States jurisdiction. Their mobility across borders gave them options unavailable to ordinary members; relocation to colony settlements was a live alternative whenever scrutiny tightened.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, elite_sealing_recipient_families, beneficiary,
    organized, generational, arbitrage, continental).

% Senior quorum members who performed post-declaration marriages in the colonies and offshore venues, kept the ordinance pipeline running, and judged which candidates qualified. Several later refused the terms of the 1904 second declaration and left the quorum under pressure; the costs of administering the covert track landed on them personally, years after the services rendered.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, sealing_performing_apostles, agenda_setter,
    powerful, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__practice_doctrine_gap, sealing_performing_apostles, payer).

% Senior quorum members who judged the public declaration a surrender and said so in council, resisting the ambiguity rather than administering it. They paid in standing: censure, removal from assignments, and in several cases resignation or release from the quorum during and just after the interval.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, manifesto_objecting_apostles, payer,
    powerful, biographical, identity_locked, continental).

% Rank-and-file members who learned of the public declaration as a binding reversal, then encountered rumors, colony marriages, and contradictory testimonies. Their tithing, migration, and political loyalty financed the arrangement; asking direct questions risked being marked as faithless. Leaving meant forfeiting community, temple standing, and the salvation framework organizing their family life.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    moderate, biographical, trapped, regional).

% Women sealed into new plural marriages during the window, frequently in colony or offshore ceremonies. Their unions occupied ambiguous legal and doctrinal standing: celebrated as covenant ordinances, deniable as facts, and subject to later repudiation or quiet dissolution. Their mobility was minimal; household, children, and reputation bound them to the outcomes.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, post_manifesto_plural_wives, payer,
    powerless, biographical, trapped, regional).

% Groups that took the preserved doctrine at face value and concluded the public declarations were temporary expedients at best. Their separation from the main body matured in the years after the interval, but the grievance chain runs directly through the window: they organized around practices and claims the ambiguity had kept nominally alive.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_schism_communities, payer,
    organized, generational, mobile, regional).

% Senate investigating committees, Justice Department attorneys, and marshals who prosecuted post-declaration offenses, subpoenaed leaders, and extracted sworn testimony. They engaged only the arrangement's public face; the sworn answers they received were shaped for them, and the discrepancy between testimony and colony records surfaced repeatedly in hearings.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_monitoring_authorities, observer,
    institutional, generational, analytical, national).

% Church-affiliated women who had campaigned against plural marriage within the community - including a documented 1886 petition - and who carried its domestic costs. They were not consulted in 1890, nor in the management of the window that followed; the arrangement's terms were set entirely in presidency and quorum councils.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, anti_polygamy_women_activists, excluded,
    moderate, biographical, trapped, regional).

% Scholars reconstructing the window from diaries, sealing registers, colony records, and hearing transcripts. They sit outside the arrangement's reward structure entirely; their accounts are the main corrective to the official record and the main source for estimating the covert track's volume.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, first_presidency_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Held two incompatible commitments together without deciding between them: public legal compliance with federal anti-polygamy enforcement, and doctrinal continuity with a canonized revelation the community understood as eternally binding. The ambiguity let ordinary members keep their identity narrative, let prominent families honor outstanding sealing obligations, and let the institution survive politically - each track proceeding on the assumption that the other would not force an accounting.
% TRANSFER_FUNCTION: Moved certainty and membership clarity away from the general membership (who absorbed bewilderment, betrayed expectations, and later the costs of schism) toward the leadership (which gained political survival, statehood leverage, and discretionary flexibility); moved marriage ordinances to well-connected families during the window; moved legal risk downward onto individual performers and participants.
% ABSENT_VOICES: Anti-polygamy women inside the church (who had petitioned against plural marriage in 1886) had no seat; younger members who would inherit the ambiguity were unrepresented; rank-and-file who deserved an honest accounting of what was being asked of them were addressed only through public declarations crafted for Congress, not for them.
% DISAPPEARANCE_RATIONALE: Had the gap vanished overnight - say, in 1893 - the institution faced an immediate forced choice with no neutral position: formal doctrinal revision of Section 132 (instant rupture with the covenant-faithful and an admission that canon is negotiable) or open defiance of federal enforcement (confiscation, arrests, loss of statehood prospects). Every major seat's arrangements depended on the ambiguity's persistence; removing it forces rearrangement in whichever direction the institution chose.
% FOUNDING_PROBLEM: Surviving federal criminalization and disfranchisement campaigns targeting plural marriage without abandoning a canonized revelation the community regarded as eternally binding.
% FOUNDING_PROBLEM_CORROBORATION: Congressional committee records (the Reed Smoot hearing testimony of 1904-1906) attest both that the acute legal crisis had eased and that post-declaration marriages and inaccurate denials occurred; independent historians working from diaries, sealing records, and colony registers corroborate the gap's existence and its outliving of the acute phase. The institution itself long denied the gap officially - the corroboration comes entirely from outside the benefiting parties, which is itself signal.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because the arrangement traded membership clarity for institutional flexibility: the covert track's volume grew while public assurances hardened, so the distance between what members were told and what was done widened monotonically. Suppression (0.62) is authored as a raw structural property - it is NOT scaled by power or scope in the engine's computation; only extractiveness is scaled, by directionality and spatial scope. The suppression figure reflects enforced silence: testimony coached for congressional inquiry, discipline threatened against disclosure, and deference norms that made direct questioning costly. Theater ratio (0.58) is elevated because the arrangement's most visible activity - public declarations, sworn answers, compliance performances staged for federal audiences - was substantially performative relative to the covert functional track running underneath. Accessibility collapse (0.55) is mid-range: the alternatives (honest doctrinal revision, open defiance, exit) remained visible but were foreclosed by the ambiguity itself - the arrangement's operation destroyed the possibility of a clear answer rather than the possibility of leaving, and leaving (taken by the schism communities) carried severe costs. Resistance (0.60) was real: objecting apostles in council, member disaffection, congressional exposure, and the eventual separation of the covenant-faithful. The measurement series run on one shared time grid (points 0-14 at step 2) with every tracked metric authored at every point. Suppression_requirement is authored as a series because the story specifically traces enforcement-capacity change: testimony management hardened into the 1904 crackdown, a rising enforcement trajectory rather than a static picture. Trajectories are monotonic, not cyclical - the gap deepened steadily until external exposure forced closure.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from the same structure. From the presidency's position the arrangement was survival management it built and controlled: every ambiguity was a tool, every denial a shield. From the general membership's position the same structure operated as betrayed trust: they financed, migrated, and voted for an institution whose canonical commitments turned out to be administratively negotiable without notice. The two apostolates show same-level lateral divergence: sealing_performing_apostles and manifesto_objecting_apostles held identical nominal power and identical identity-locked exits, yet one group administered the covert track and the other paid in standing for refusing it - role uptake, not power, differentiated their experience. Inter-institutionally, the federal monitoring authorities engaged only the arrangement's public face: they saw compliance theater and extracted testimony shaped for them, remaining blind to the colony-track functionality that members experienced directly. Coalition note: the membership's latent class capacity (organized-scale grievances, shared information shocks when colony marriages surfaced) was fragmented by deference norms and information asymmetry - the suppression mechanism worked precisely by preventing the coalition from recognizing itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency sits near the beneficiary end: it collects the arrangement's gains (continuity, statehood, discretion) and its identity-lock binds it to the arrangement it administers. Elite sealing recipient families also sit near the beneficiary end, pushed further by arbitrage-grade exit - jurisdictional mobility let them consume the ordinance while externalizing legal risk. General membership and post-declaration plural wives sit near the full-target end: trapped payers absorbing clarity-loss, legal ambiguity, and later repudiation. The objecting apostles carry high directionality despite institutional-grade power - they paid in censure and resignation, and their identity lock prevented exit short of rupture. The performing apostles occupy a genuine middle: they administered the extraction and deferred its costs onto themselves. The federal authorities and institutional historians are analytical seats outside the extraction arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - surviving federal criminalization without abandoning a canonized revelation - was substantially dead by 1896-1898: statehood arrived, prosecutions wound down, the acute existential threat passed. Yet the gap persisted another six to eight years, sustained by inertia, elite interest in continued sealings, and the escalating impossibility of admitting that prior sworn denials had been false. That overshoot is the mandatrophy signature: the arrangement outlived its mandate because fixing it had become prohibitively costly for the only seat that could fix it - honest closure required confessing perjury-adjacent testimony, rupturing the covenant-faithful, and destabilizing the authority claims the gap had been protecting. The classification guards against both mislabels: not a snare, because a genuine coordination function operated throughout (identity continuity, transition management, elite obligation honoring) and exit was never fully suppressed - the schism proves alternatives remained reachable; not a rope, because the extraction was sharply asymmetric with identifiable victims and required active enforcement of silence. Nor is it a piton candidate: a concentrated capturer (the presidency) demonstrably receives the gains, placing this in the capture cell rather than the diffuse-cost cell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This story instantiates the practice_doctrine_gap reading of kernel marriage_commitment_reversal: is the maintained doctrine-practice decoupling itself the operative constraint, rather than the causal trigger (endogenous revelation) or the coercive override (exogenous force) that the sibling readings foreground?',
    'Comparative classification across the three sibling stories under identical structural data: if the sibling readings classify differently, the divergence locates the disagreement in causal attribution rather than structure; convergence confirms the standing ambiguity as the load-bearing element.',
    'Adopting the endogenous reading would lower measured deception (a revelation-legitimated suspension) and shift victim emphasis toward those who doubted the revelation''s authenticity; adopting the exogenous reading would raise external-coercion weighting and recast leadership as compelled rather than strategic. This reading keeps the ambiguity itself as the assessed arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Which reading of the marriage-commitment-reversal kernel this story instantiates and what sibling adoption would change.').

omega_variable(
    post_manifesto_marriage_count,
    'How many plural marriages were actually performed between the 1890 public declaration and the 1904 second declaration, inside and outside the claimed-legal jurisdictions?',
    'Opening and auditing sealing and temple records, colony registers (northern Mexico, southern Alberta), maritime and offshore records, cross-checked against hearing testimony and posthumous memoirs.',
    'Counts near the upper estimates (200+) confirm sustained dual-track operation and support high extraction; counts near zero would collapse this reading toward clean compliance and lower epsilon materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_marriage_count, empirical, 'True volume of post-declaration plural marriages sustaining the covert track.').

omega_variable(
    denial_ethics_attribution,
    'Were the sworn public denials before congressional inquiry institutional self-protection against prosecution of ordinary members, or self-serving concealment protecting leadership authority?',
    'Counterfactual cost analysis at each denial point: what disclosure would have cost members versus leaders; comparison with contemporaneous institutions facing similar investigations.',
    'If necessity dominates, suppression reads as externally imposed and leadership culpability drops; if concealment dominates, suppression is internally chosen and effective extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denial_ethics_attribution, preference, 'Moral attribution of the gap''s enforced silence.').

omega_variable(
    schism_causality_horizon,
    'Does the victim set properly include the fundamentalist schism communities whose separation matured after the interval, given the gap-era ambiguity as proximate cause?',
    'Documentary tracing of schism organizers'' cited grievances back to gap-era events; temporal-lag analysis of when the ambiguity entered their formative commitments.',
    'Including them widens the victim set and raises effective extraction; excluding them confines victims to contemporaneous membership and lowers it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(schism_causality_horizon, empirical, 'Whether the delayed schism counts as a victim effect of the gap.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression primarily structural (federal threat and legal exposure disciplining disclosure) or internalized (deference to prophetic authority making member questioning socially unthinkable)?',
    'Post-interval trajectory: as the external threat receded after 1904, did member-side silence persist at comparable levels (internalized component) or relax (structural component)?',
    'If substantially internalized, effective suppression exceeds the structural measure and outlasts the interval; if structural, suppression should decay with the external threat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, conceptual, 'Structural versus internalized components of the gap''s enforced silence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0, 0.34).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t2, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 2, 0.38).
narrative_ontology:measurement_basis(marr_tr_t2, observed).
narrative_ontology:measurement(marr_tr_t4, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 4, 0.42).
narrative_ontology:measurement_basis(marr_tr_t4, observed).
narrative_ontology:measurement(marr_tr_t6, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 6, 0.46).
narrative_ontology:measurement_basis(marr_tr_t6, observed).
narrative_ontology:measurement(marr_tr_t8, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 8, 0.5).
narrative_ontology:measurement_basis(marr_tr_t8, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 10, 0.53).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t12, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 12, 0.56).
narrative_ontology:measurement_basis(marr_tr_t12, observed).
narrative_ontology:measurement(marr_tr_t14, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 14, 0.58).
narrative_ontology:measurement_basis(marr_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t2, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 2, 0.56).
narrative_ontology:measurement_basis(marr_be_t2, observed).
narrative_ontology:measurement(marr_be_t4, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 4, 0.6).
narrative_ontology:measurement_basis(marr_be_t4, observed).
narrative_ontology:measurement(marr_be_t6, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 6, 0.63).
narrative_ontology:measurement_basis(marr_be_t6, observed).
narrative_ontology:measurement(marr_be_t8, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 8, 0.66).
narrative_ontology:measurement_basis(marr_be_t8, observed).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 10, 0.69).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t12, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 12, 0.72).
narrative_ontology:measurement_basis(marr_be_t12, observed).
narrative_ontology:measurement(marr_be_t14, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 14, 0.74).
narrative_ontology:measurement_basis(marr_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t2, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 2, 0.45).
narrative_ontology:measurement_basis(marr_su_t2, observed).
narrative_ontology:measurement(marr_su_t4, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 4, 0.48).
narrative_ontology:measurement_basis(marr_su_t4, observed).
narrative_ontology:measurement(marr_su_t6, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 6, 0.51).
narrative_ontology:measurement_basis(marr_su_t6, observed).
narrative_ontology:measurement(marr_su_t8, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(marr_su_t8, observed).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 10, 0.57).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t12, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(marr_su_t12, observed).
narrative_ontology:measurement(marr_su_t14, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 14, 0.62).
narrative_ontology:measurement_basis(marr_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the 1890 Manifesto reversal' decomposes into three structurally distinct constraints per the epsilon-invariance principle: a causal-attribution pair (endogenous revelation vs exogenous coercion) and a standing-structure claim (the maintained doctrine-practice gap, this story). Epsilon differs across the family because the referent differs: this reading authors epsilon for the standing ambiguous arrangement itself (high - clarity sacrificed, covert operations, enforced silence); the endogenous reading would author epsilon for a revelation-governed transition; the exogenous reading for a coerced capitulation. The gap's documented operation (covert marriages, exposed denials) feeds back into how both causal siblings are defended, which is why this story links to both via affects_constraints and via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
