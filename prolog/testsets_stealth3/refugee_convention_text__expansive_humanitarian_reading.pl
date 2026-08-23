% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Expansive Humanitarian Reading of the Refugee Convention
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'the 1951
 *   Refugee Convention' (with its 1967 Protocol): the expansive humanitarian
 *   reading, under which the Convention is an unbendable humanitarian mandate
 *   — 'well-founded fear of persecution' reaches generalized violence and
 *   non-state predation, 'particular social group' reaches gender, sexual
 *   orientation and gender identity, and clan or kin categories, and
 *   interdiction at sea, pushback, and offshore transfer are classified as
 *   refoulement. Per the epsilon-referent rule for kernel readings, epsilon
 *   is authored for the standing arrangement THIS reading describes — states
 *   bound by the broad mandate as it actually operates — assessed by this
 *   reading's own lights, which counts most state-side costs as owed duty
 *   rather than takings. The sibling readings
 *   (restrictive_sovereignty_reading, procedural_integrity_reading) are
 *   separate constraints in separate files; nothing about them is averaged
 *   into this epsilon. Claim and metrics are authored independently: the
 *   claimed type records the structure I believe true of this reading's
 *   arrangement (a genuine coordination core carrying real, actively
 *   enforced, asymmetrically distributed costs), while each metric records
 *   what I believe descriptively true of its operation. KEY AGENTS (by
 *   structural relationship): - asylum_seekers_and_refugees: primary
 *   protected class (powerless/trapped) — the subsidized seat -
 *   frontline_host_states: primary cost bearer (institutional/constrained) —
 *   highest-directionality payer - destination_state_executives: cost-bearing
 *   but mobile seat (powerful/arbitrage) — pays while constructing exit
 *   routes the reading condemns - unhcr: administering interpreter
 *   (institutional/identity_locked) — operationalizes breadth; collects
 *   mandate and budget - refugee_rights_advocacy_sector: secondary
 *   beneficiary (organized/identity_locked) — supplies the doctrinal argument
 *   - regional_courts_treaty_bodies: enforcement seat
 *   (institutional/constrained) — converts breadth into binding holdings -
 *   receiving_local_communities: diffuse local payer (moderate/constrained) -
 *   interdiction_offshore_operators: excluded seat — its function is what the
 *   reading defines as violation - non_accession_destination_states: excluded
 *   seat — outside the obligation entirely
 *
 * KEY AGENTS:
 *   - asylum_seekers_and_refugees: primary protected class (powerless/trapped) — subsidized seat; recognition depends entirely on this reading's broad criteria
 *   - frontline_host_states: primary cost bearer (institutional/constrained) — concentrated protection burden, no lawful offloading route
 *   - destination_state_executives: cost-bearing but mobile (powerful/arbitrage) — pays compliance and litigation costs while building externalization routes
 *   - unhcr: administering interpreter (institutional/identity_locked) — issues the guidelines that operationalize breadth; collects mandate, budget, and supervisory authority
 *   - refugee_rights_advocacy_sector: secondary beneficiary (organized/identity_locked) — caseload, funding, and professional identity track doctrinal breadth
 *   - regional_courts_treaty_bodies: enforcement seat (institutional/constrained) — converts breadth into precedent
 *   - receiving_local_communities: diffuse local payer (moderate/constrained) — service strain without treaty seat
 *   - interdiction_offshore_operators: excluded (institutional/trapped) — function defined as violation
 *   - non_accession_destination_states: excluded (powerful/arbitrage) — outside the obligation, objection unregistered
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.32).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.58).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Expansive Humanitarian Reading of the Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '11f59bd7-fa7d-46f3-981d-998ba5a377a5').
narrative_ontology:cs_kernel_codification('11f59bd7-fa7d-46f3-981d-998ba5a377a5', fixed_text).
narrative_ontology:cs_authority_grounding('11f59bd7-fa7d-46f3-981d-998ba5a377a5', lineage).
narrative_ontology:cs_interpretation_layer_present('11f59bd7-fa7d-46f3-981d-998ba5a377a5').
narrative_ontology:cs_reading_relation('11f59bd7-fa7d-46f3-981d-998ba5a377a5', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('11f59bd7-fa7d-46f3-981d-998ba5a377a5', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('11f59bd7-fa7d-46f3-981d-998ba5a377a5', foundational, non_refoulement_is_absolute_and_unbendable).
narrative_ontology:cs_axiom_status(non_refoulement_is_absolute_and_unbendable, holdable).
narrative_ontology:cs_axiom_grounding('11f59bd7-fa7d-46f3-981d-998ba5a377a5', non_refoulement_is_absolute_and_unbendable, deontological).
narrative_ontology:cs_axiom('11f59bd7-fa7d-46f3-981d-998ba5a377a5', foundational, protection_extends_to_generalized_violence_and_identity_groups).
narrative_ontology:cs_axiom_status(protection_extends_to_generalized_violence_and_identity_groups, holdable).
narrative_ontology:cs_axiom_grounding('11f59bd7-fa7d-46f3-981d-998ba5a377a5', protection_extends_to_generalized_violence_and_identity_groups, deontological).
narrative_ontology:cs_reference_frame('11f59bd7-fa7d-46f3-981d-998ba5a377a5', unbendable_humanitarian_mandate).
narrative_ontology:cs_drift_state('11f59bd7-fa7d-46f3-981d-998ba5a377a5', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('11f59bd7-fa7d-46f3-981d-998ba5a377a5', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_and_refugees).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, unhcr).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugee_rights_advocacy_sector).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, frontline_host_states).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_state_executives).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, receiving_local_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, destination_state_executives).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_absolute_norm).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, psg_includes_gender_sexual_orientation_clan).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, generalized_violence_counts_as_persecution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee persecution, generalized violence, or identity-based harm and seek recognition under the Convention's broad categories — gender-based persecution, sexual orientation and gender identity, clan and kin-based social groups. What flows to them is status, non-return, and access to residence and services; what flows from them is nothing they control, since their claims must be substantively assessed wherever they arrive. Exiting the need for protection is not possible — flight relocates the need rather than ending it.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_and_refugees, beneficiary,
    powerless, immediate, trapped, global).

% Issues the interpretive guidelines that give 'well-founded fear' and 'particular social group' their operational breadth, supervises state practice, and conducts status determination in many operations. Its budget and institutional reach expand with the breadth of the categories it administers, while its funding depends on the same states whose practice it criticizes. Repudiating the breadth of the mandate would dissolve the agency's own role.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, unhcr, beneficiary).

% Litigators, NGOs, and scholars who develop and defend the broad doctrinal reading before courts and treaty bodies. Caseload, funding, and scholarly terrain all track how far the categories reach; the sector's professional identity is built around defending that reach, and arguing it narrower would mean abandoning the profession's core project.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_rights_advocacy_sector, beneficiary,
    organized, biographical, identity_locked, global).

% First-arrival states such as Jordan, Lebanon, Turkey, Kenya, and Colombia absorb large displaced populations under the broad categories. Housing, schooling, health care, and security costs concentrate on their territory while recognition outcomes bind them through courts and treaty supervision. They cannot refuse entry to groups the reading protects, cannot push arrivals back across a frontier without triggering refoulement findings, and depend partly on donor funding they do not control.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, frontline_host_states, payer,
    institutional, biographical, constrained, regional).

% Governments of wealthier destinations are bound by expansive jurisprudence through their own courts, where gender, sexuality, and clan-based claims are routinely recognized. They carry litigation defeats, fiscal costs, and political backlash, and respond by building routes around the obligation: safe-third-country agreements, offshore processing, maritime interdiction. Each route is challenged as refoulement, yet the capacity to keep building new routes is real. They also draw second-order benefits from shared standards and from labor inflows their economies absorb.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_state_executives, payer,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, destination_state_executives, beneficiary).

% Towns and municipalities where recognized refugees are settled carry the housing, school-place, and service strain that national-level decisions produce. They hold no seat in the treaty system, cannot refuse placements, and their principal lever is political pressure directed upward at national governments.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, receiving_local_communities, payer,
    moderate, biographical, constrained, local).

% Navies, coast guards, and contractors running interception-at-sea and offshore-processing arrangements. Their core function — turning vessels around, transferring arrivals to third territories — is precisely what this framework's jurisprudence classifies as refoulement, which places them outside the legitimate conversation. Their operational expertise in search-and-rescue and deterrence finds no hearing inside the mandate's logic.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, interdiction_offshore_operators, excluded,
    institutional, immediate, trapped, regional).

% Wealthy labor-importing states that never acceded to the Convention or its Protocol stand wholly outside the obligation. They recruit large migrant workforces under sponsorship systems carrying no protection duty, and would object that accession concentrates the costs of a global protection order on signatories while they take labor and pay nothing — an objection the treaty system has no mechanism to register.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, non_accession_destination_states, excluded,
    powerful, generational, arbitrage, regional).

% Regional courts, UN treaty bodies, and national apex courts that convert the broad reading into binding holdings — interception on the high seas, gender-based persecution, sexual-orientation and clan social groups. They administer the reading case by case; precedent locks each holding in place, and retreating from established breadth carries institutional cost they rarely accept.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, regional_courts_treaty_bodies, agenda_setter,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_and_refugees).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a race-to-the-bottom in asylum — each state deflecting arrivals onto its neighbors — into a common protection standard: uniform criteria for who qualifies, a shared non-refoulement restraint, and predictable adjudication that lets states, courts, and agencies coordinate without re-bargaining from zero at every crisis.
% TRANSFER_FUNCTION: Moves protection status, adjudication effort, and fiscal resources from signatory states — concentrated on first-arrival frontline hosts — to individuals recognized under the broad persecution categories; secondarily moves doctrinal authority toward courts and the supervising agency, and caseload toward the advocacy sector.
% ABSENT_VOICES: Refugees themselves hold no formal seat anywhere in the treaty system; courts and advocates speak for them. Interdiction navies, offshore-processing operators, and non-accession destination states object from outside the framework entirely. Frontline hosts are systematically under-weighted in burden-sharing negotiation relative to the costs they carry, so even the seated payers are partially unheard.
% DISAPPEARANCE_RATIONALE: Recognition collapses toward narrow individualized state-persecution proofs; generalized-violence and identity-based claims lose their doctrinal home; interception at sea and offshore transfer resume without legal finding; frontline states lose the court-made shield that distributes their burden; supervisory machinery, social-group jurisprudence, and the entire advocacy complex unwind around a mandate that no longer exists.
% FOUNDING_PROBLEM: After the Second World War, mass displacement met purely discretionary charity: no legal category obliged any state to admit or protect a person fleeing persecution, and the persecuted could be returned to their persecutors at the border official's discretion.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: World Bank and ICRC displacement data, the forced-migration research literature, and the burden-sharing appeals of frontline host states — who are payers, not beneficiaries — all attest that mass flight from persecution and generalized violence persists at scale. No source outside the benefiting parties attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.32 — moderate — because the referent is this reading's own arrangement assessed by its own lights: the bulk of what states surrender (admission discretion, fiscal transfers, adjudication effort) is counted as owed duty, leaving residual unjustified taking concentrated in apparatus growth and rigidity costs. Suppression (0.58) is authored as a raw, unscaled structural property: the mandate actively forecloses state alternatives — interdiction, offshore transfer, narrow PSG criteria — through treaty-body findings, court judgments, and supervisory criticism, but stops short of hard coercion since denunciation and reservations remain formally open. Theater ratio 0.30: adjudication and precedent enforcement are functional, while a visible minority of activity (commemoration, pledge summits, resolutions against non-compliant practice) performs the mandate rather than executing it. Accessibility collapse 0.50: alternatives remain exercisable — externalization agreements, non-accession, safe-third-country arrangements — even after the reading renders them unlawful. Resistance 0.78: the constraint meets the heaviest sustained resistance in the international-law band — pushback campaigns, externalization treaties, reservation regimes, funding leverage. All three series run on ONE shared seven-point grid (1951–2026) as the alignment rule requires. The suppression_requirement series is tracked deliberately rather than left static because the story's enforcement picture changes materially: enforcement capacity built steadily from the Protocol through the Hirsi-era litigation wave, which is a real enforcement-history trajectory, not noise. The mild post-2012 dip in base_extractiveness models efficiency pressure and funding shortfalls trimming apparatus overhead while the mandate's scope held.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply divergent types across seats from the structural data alone. From the frontline_host_states seat — victim-declared, institutional power, constrained exit — effective extraction is amplified toward the full-target end: the mandate arrives as unbendable obligation stacked on geographically unavoidable arrival. From destination_state_executives — equally victim-declared but holding arbitrage-grade exit — the same structure computes with damped severity: they experience it as a contest they can litigate around and route past, closer to a negotiable rule than a trap. From asylum_seekers_and_refugees and the advocacy sector — beneficiaries, trapped or identity-locked — the identical structure computes as lifeline. Same-level dynamics: frontline and destination executives hold nominally identical power as sovereign treaty parties, yet sit at opposite exit poles; the differentiation is constraint-specific (arrival geography), not power-specific. Inter-institutional dynamics: courts and the agency administer the reading and sit nearer symmetry; executives resist it; the excluded seats hold objections the framework provides no mechanism to hear, which is why unanimity inside the system is partly an artifact of who was admitted to the room.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive directionality toward the subsidized end: asylum_seekers_and_refugees (beneficiary, trapped — the arrangement is their protection and they cannot leave it), refugee_rights_advocacy_sector (beneficiary, identity_locked — maximal lock-in on the beneficiary side), unhcr (dual-positioned administrator-collector, held low but nonzero by its secondary beneficiary position). Victim declarations drive it toward the target end: frontline_host_states (constrained exit keeps them near full-target), receiving_local_communities (local, constrained). destination_state_executives are victim-declared but arbitrage exit pulls them down from the trapped-target band toward the middle-high range — they pay, but they can shop jurisdictions and build workarounds. regional_courts_treaty_bodies carry no beneficiary or victim declaration and revert to the canonical institutional fallback, which suits an enforcer that neither collects nor pays. No directionality_overrides are authored: the derivation chain from declarations plus exit atoms already produces the correct ordering, and adding overrides would duplicate structural data the declarations express.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass persecution meeting purely discretionary charity — remains live, corroborated from outside the beneficiary set by displacement data and the burden-sharing appeals of frontline states, who are payers rather than beneficiaries; the disappearance verdict is world_rearranges. The mismatch consumer therefore finds no dead-mandate signal, and none is authored. The genuine lifecycle risk runs the other way: if state resistance completes its shift from losing in court to accomplishing externalization, enforcement could decay into ceremonial condemnation — the signature to watch is theater_ratio continuing to climb alongside a falling suppression_requirement, which would mark drift toward a theatrically maintained mandate. Classification discipline cuts both ways: reading this arrangement as pure coordination erases the concentrated, actively enforced costs sitting on trapped state seats; reading it as pure taking erases the real collective-action function and the subsidized majority it serves. The tangled_rope claim holds both faces in the measurement, which is exactly the information a later reader needs to distinguish repairable burden misallocation from structural extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_reading_of_contested_kernel,
    'This constraint instantiates only the expansive humanitarian reading of the refugee_convention_text kernel — would the restrictive_sovereignty_reading or procedural_integrity_reading instantiate a different constraint with a different epsilon, victim set, and classification?',
    'Compile the sibling readings as separate stories and compare computed classifications. The disagreement is located in three structural elements: the bendability of the mandate, the ''well-founded fear'' threshold (individualized state persecution versus generalized violence and non-state actors), and the boundary of ''particular social group'' (immutable characteristics with state awareness versus gender, LGBTQ+, and clan categories), plus the legal status of interdiction and offshore transfer.',
    'Under the restrictive reading the victim set contracts to individualized state-persecution claims, state-facing effective extraction falls, and the arrangement plausibly reclassifies toward the rope/snare boundary; this file''s epsilon and beneficiary/victim structure do not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_reading_of_contested_kernel, conceptual, 'Kernel-contest routing: one of three readings of the Convention text; structural delta is the broad victim set, refoulement extended to interdiction and offshore transfer, and the duty of substantive assessment of every claim.').

omega_variable(
    frontline_burden_extraction_or_underdelivery,
    'Is the cost concentration on frontline host states extraction produced by the mandate''s operation, or under-delivery of the burden-sharing component the mandate presupposes?',
    'Counterfactual comparison against a functioning burden-sharing instrument: if frontline costs fall proportionately as sharing mechanisms activate without any contraction in protection granted, the concentration is a design gap in delivery, not a taking by the mandate.',
    'If extraction, the tangled_rope profile tilts toward the snare boundary and the state-seat extraction measure rises; if under-delivery, the arrangement keeps its coordination character with a distributive defect to be repaired rather than removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontline_burden_extraction_or_underdelivery, empirical, 'Whether frontline cost concentration is a product of the constraint or a missing complement to it.').

omega_variable(
    protection_complex_rent_share,
    'How much of the growth in the protection apparatus (agency operations, guideline production, litigation volume) is necessary coordination cost of the broad mandate versus rent captured by the sectors the breadth sustains?',
    'Cost-per-recognized-person trend analysis against comparable protection operations outside the Convention frame, plus audit of guideline and litigation output against recognition outcomes.',
    'A large rent share pushes epsilon above the authored 0.32 and strengthens the hybrid-coordination account; a small share supports the coordination-first reading and narrows the seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_complex_rent_share, empirical, 'Rent-versus-overhead split inside the protection complex that the broad reading sustains.').

omega_variable(
    owed_duty_vs_taken_resource,
    'Are the costs the mandate imposes on signatory states owed humanitarian duties (this reading''s assessment) or resources taken from sovereign discretion (the restrictive reading''s assessment)?',
    'Irreducible without adopting a prior normative framework; the observable proxy is which framework governs in each jurisdiction''s court practice and treaty politics over time.',
    'Determines whether state-seat extraction registers as legitimate price or as taking; the consequence is per-seat rather than constraint-wide, since the structural declarations (who pays, who collects) are unchanged either way.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(owed_duty_vs_taken_resource, preference, 'Normative status of the state-side cost burden; the deepest axis of the kernel contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcehr_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(rcehr_tr_t1967, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(rcehr_tr_t1984, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1984, 0.16).
narrative_ontology:measurement(rcehr_tr_t1998, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(rcehr_tr_t2012, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2012, 0.27).
narrative_ontology:measurement(rcehr_tr_t2019, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement(rcehr_tr_t2026, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(rcehr_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.14).
narrative_ontology:measurement(rcehr_be_t1967, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1967, 0.19).
narrative_ontology:measurement(rcehr_be_t1984, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1984, 0.24).
narrative_ontology:measurement(rcehr_be_t1998, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1998, 0.29).
narrative_ontology:measurement(rcehr_be_t2012, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2012, 0.34).
narrative_ontology:measurement(rcehr_be_t2019, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2019, 0.33).
narrative_ontology:measurement(rcehr_be_t2026, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2026, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(rcehr_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.2).
narrative_ontology:measurement(rcehr_su_t1967, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1967, 0.28).
narrative_ontology:measurement(rcehr_su_t1984, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1984, 0.34).
narrative_ontology:measurement(rcehr_su_t1998, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1998, 0.42).
narrative_ontology:measurement(rcehr_su_t2012, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2012, 0.52).
narrative_ontology:measurement(rcehr_su_t2019, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement(rcehr_su_t2026, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2026, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Refugee Convention' decomposes into at least three structurally distinct constraints with different epsilon, victim sets, and enforcement profiles: this expansive humanitarian reading; restrictive_sovereignty_reading (minimum floor, maximal discretion); procedural_integrity_reading (process integrity as the invariant core). All three share the upstream fixed kernel text; each is authored as its own story per the epsilon-invariance principle, and the family links run through network.affects_constraints in all three files. The upstream fixed text lends its stability to the downstream interpretive contest, which is where the extraction-relevant variance lives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
