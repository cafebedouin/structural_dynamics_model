% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [SUPERSEDED_JUDICIALLY (District of Columbia v. Heller, 2008)]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment — Collective-Rights Reading (State Militia Authority)
 *   domain: legal/constitutional/political philosophy
 *
 * SUMMARY:
 *   This story instantiates the collective-right reading of the Second
 *   Amendment as a standing constitutional arrangement: the construction,
 *   dominant in federal court from Presser v. Illinois (1886) to District of
 *   Columbia v. Heller (2008), under which the amendment protects state
 *   authority over organized militias and confers no constitutional
 *   protection on private weapon ownership outside militia service. The
 *   arrangement's operation is twofold: it shields state military
 *   institutions from federal dissolution (a genuine federalism function),
 *   and it channels every individual protection claim into a dismissal
 *   structure that returns nothing to the claimant while confirming state
 *   regulatory autonomy. The epsilon referent is the standing arrangement
 *   itself — the amendment as collectively read — assessed by this reading's
 *   own lights: prohibition measures carry near-zero epsilon from this seat
 *   because individuals hold no protected interest for prohibitions to
 *   violate (the reading's expected structural delta); the measured epsilon
 *   arises instead from the protection-allocation asymmetry and the
 *   absorption of individual claims, not from the prohibitions the reading
 *   permits. The claim/metric gap is deliberate and independent: the reading
 *   is claimed as tangled_rope on structural grounds (genuine coordination
 *   core plus asymmetric allocation plus active judicial enforcement), while
 *   the metrics describe the arrangement's actual operation across its
 *   lifecycle, including the nine decades it persisted after its founding
 *   object was federalized out of existence. KEY AGENTS (by structural
 *   relationship): - state_governments: Primary beneficiary
 *   (institutional/constrained) — hold the militia-authority shield and
 *   unbounded regulatory discretion - federal_judiciary: Agenda-setter
 *   (institutional/analytical) — administers and enforces the reading through
 *   adjudication - individual_firearm_owners: Primary target
 *   (moderate/constrained) — bear regulation with no constitutional floor -
 *   arms_rights_litigants: Secondary target (organized/mobile) — claims
 *   absorbed and dismissed by the structure - federal_government: Bound party
 *   (institutional/constrained) — ceded militia authority as founding-bargain
 *   price - national_guard_institutions: Nominal protected object
 *   (institutional/trapped) — shielded in name, hollowed by federalization -
 *   freedmen_disarmed_populations: Excluded voice (powerless/trapped) — bore
 *   disarmament with no seat in the framework -
 *   constitutional_law_scholarship: Analytical observer
 *   (analytical/analytical) — documented the drift and built the reversal
 *   campaign
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.43).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.68).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.43).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment — Collective-Rights Reading (State Militia Authority)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "legal/constitutional/political philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__collective_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '2291cc19-c9c7-4320-9163-fa112a5b58e5').
narrative_ontology:cs_kernel_codification('2291cc19-c9c7-4320-9163-fa112a5b58e5', fixed_text).
narrative_ontology:cs_authority_grounding('2291cc19-c9c7-4320-9163-fa112a5b58e5', lineage).
narrative_ontology:cs_interpretation_layer_present('2291cc19-c9c7-4320-9163-fa112a5b58e5').
narrative_ontology:cs_reading_relation('2291cc19-c9c7-4320-9163-fa112a5b58e5', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('2291cc19-c9c7-4320-9163-fa112a5b58e5', second_amendment_arms_right__civic_republican_reading, forecloses).
narrative_ontology:cs_axiom('2291cc19-c9c7-4320-9163-fa112a5b58e5', foundational, prefatory_clause_controls_right_holder).
narrative_ontology:cs_axiom_status(prefatory_clause_controls_right_holder, holdable).
narrative_ontology:cs_axiom_grounding('2291cc19-c9c7-4320-9163-fa112a5b58e5', prefatory_clause_controls_right_holder, conventional).
narrative_ontology:cs_axiom('2291cc19-c9c7-4320-9163-fa112a5b58e5', secondary, private_arms_outside_militia_within_plenary_police_power).
narrative_ontology:cs_axiom_status(private_arms_outside_militia_within_plenary_police_power, holdable).
narrative_ontology:cs_axiom_grounding('2291cc19-c9c7-4320-9163-fa112a5b58e5', private_arms_outside_militia_within_plenary_police_power, conventional).
narrative_ontology:cs_reference_frame('2291cc19-c9c7-4320-9163-fa112a5b58e5', founding_era_militia_federalism).
narrative_ontology:cs_drift_state('2291cc19-c9c7-4320-9163-fa112a5b58e5', national_guard_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2291cc19-c9c7-4320-9163-fa112a5b58e5', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, national_guard_institutions).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_firearm_owners).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, arms_rights_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, federal_government).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, federal_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organize, arm, and discipline military forces under their own authority and regulate private weapons within their borders. The reading shields their military institutions from federal dissolution or absorption and leaves their weapons regulation free of any federal constitutional floor. They argue for the reading in litigation and defend it against individual-rights claims; their regulatory autonomy depends on the reading continuing to hold.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Adjudicates every claim brought under the amendment. Through the Presser and Miller line of decisions it dismisses individual protection claims for want of a militia nexus and sustains state weapons prohibitions as enacted. It maintains the arrangement through citation practice and precedent-stacking, collects no material benefit from what it administers, and cannot leave its adjudicative position.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Keep weapons for personal defense, hunting, and collection. Under the reading they hold no constitutional protection: regulation of their possessions faces no judicial scrutiny, and registration, licensing, or prohibition schemes stand as enacted. Moving to another state changes which regulations apply but not the absence of any constitutional floor.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_firearm_owners, payer,
    moderate, biographical, constrained, national).

% Bring suits and fund advocacy seeking individual protection under the amendment. For over a century the structure returned dismissals: claims failed for lack of militia connection, and the resources spent on them ended in precedents confirming state authority. Advocacy organizations periodically redirected effort toward state constitutions and electoral politics before returning to federal litigation.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, arms_rights_litigants, payer,
    organized, biographical, mobile, national).

% Holds enumerated authority to organize, arm, and discipline the militia but may not dissolve state forces or absorb them wholesale. The limitation was ceded at founding as the price of the federal bargain; Congress exercises its militia powers jointly with the states rather than exclusively, and shares in the union stability the bargain purchases.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, federal_government, beneficiary).

% Serve as the organized militia the reading names as its protected object. Since the 1903 and 1916 acts they drill under federal funding, hold federal muster, and deploy under federal command, so the state military independence the shield presupposes survives mostly in form. They continue to be cited as the arrangement's protected beneficiary while operating inside the federal structure.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, national_guard_institutions, beneficiary,
    institutional, generational, trapped, national).

% Freedmen and other groups subjected to state disarmament campaigns during and after Reconstruction sought federal constitutional protection for bearing arms and were denied any seat: the reading recognized no individual holder, Southern militia rolls excluded them, and the federal courts offered no forum. Their disarmament proceeded under the same state police power the reading left unbounded.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, freedmen_disarmed_populations, excluded,
    powerless, biographical, trapped, regional).

% Documents the distance between the founding-era militia system the text presupposes and the professionalized, federally integrated Guard that replaced it; catalogs the citation practices by which the reading sustained itself; and mounted the critique campaign that preceded the 2008 overruling.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, constitutional_law_scholarship, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains divided control of organized armed force between federation and member states: each level can field military institutions the other cannot dissolve, so no single government monopolizes coercion. Stated without evaluation: the arrangement solves the concentration-of-force problem by reserving militia authority to the states against federal absorption.
% TRANSFER_FUNCTION: Moves constitutional immunity from federal interference to state governments and their military institutions; moves the entire regulatory exposure of private weapon ownership onto individuals, who receive no constitutional floor; and moves individual protection claims into a dismissal structure that consumes them and returns precedential confirmation of state authority.
% ABSENT_VOICES: Freedmen and other disarmed populations during and after Reconstruction would have objected that the reading left them to state disarmament with no federal forum; individual owners outside militia service had no seat at all — the reading's framework gave their interests no standing to be voiced within constitutional argument. Both groups appear in this story as excluded or paying seats, not as participants.
% DISAPPEARANCE_RATIONALE: State regulatory autonomy over weapons, the dismissal structure processing individual claims, and the allocation of constitutional protection all depended on the reading: overnight removal would have opened individual claims to merits review, forced state prohibitions to defend themselves against a federal floor, and redistributed the protection the arrangement reserved to state institutions.
% FOUNDING_PROBLEM: Securing the states' portion of armed force: the amendment answered federal power under Article I, Section 8 to organize, arm, and discipline the militia, guaranteeing that Congress could not dissolve or absorb state military institutions and leave the states disarmed within the federal balance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: standard military-institutional histories of the 1903 Dick Act and the 1916 National Defense Act document the conversion of the state militias into a federally funded and commanded National Guard; pre-Heller scholarship across interpretive camps — including authors sympathetic to the collective reading — acknowledged that the modern Guard is not the founding-era militia the text presupposes; and individual-right advocates built their case on precisely this mismatch. No state government attests that the founding problem remains live in its original form; states defend the arrangement instead as regulatory-autonomy doctrine.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.43, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Maturity-phase scores are anchored to the 1994 column of the shared measurement grid — the arrangement at its most contested operative moment — not to the terminal 2008 column, which records judicial supersession: enforcement demand collapses (0.68 to 0.18) while theater spikes (0.58 to 0.71) as recitation outlives application. Base extractiveness (0.43) is moderate rather than high because the arrangement transfers no asset stream: it allocates constitutional protection wholly to state institutions and returns dismissals to individual claimants; the extraction is foreclosure and absorption, not taxation. Suppression (0.68) is predominantly structural — doctrinal foreclosure with no courtroom path to individual protection — with a minority internalized component: generations of lower-court practice treated individual claims as categorically frivolous before merits review. Accessibility collapse (0.55) is partial: courtroom alternatives were foreclosed uniformly, but scholarly alternatives stayed visible throughout, which is what made eventual reversal possible. Resistance (0.62) reflects a century of continuous litigation and a critique campaign that ultimately succeeded — evidence that coalition power among the paying seats, though slow to accumulate, was sufficient to break the arrangement. The measurement series run on one shared seven-point grid (1886-2008) with every tracked metric authored at every point; the trajectory is monotonic drift with terminal collapse rather than cyclical, so no oscillation mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience the same text differently. From the state seat the arrangement is a shield that regulatory autonomy was built on; from the individual-owner seat it is a closed door — regulation without floor, claims without forum. The judiciary seat administers the arrangement without collecting from it: its experience is custodial, neither subsidized nor burdened. The federal seat bears a bounded action space it ceded voluntarily at founding as bargain-price, compensated by the union stability the bargain purchases. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments are declared beneficiaries and sit near the subsidized end; their constrained exit (no exit from the constitutional order) keeps them invested. Individual firearm owners and arms-rights litigants are declared victims and sit near the full-target end, with constrained exit amplifying their effective burden. The judiciary, though the enforcing agenda-setter, appears in no beneficiary or victim declaration and derives a near-symmetric directionality — it administers without collecting. The federal government is a bound party outside the victim declaration: its cost is forgone authority priced into the founding bargain, not an ongoing transfer. No directionality overrides are used: role and exit declarations already place every seat, and the override mechanism keys on power atoms, which would misfire here because the institutional-power seats (states, judiciary, federal government, Guard) diverge sharply in structural relationship despite sharing an atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting independent state militias from federal dissolution — died between the 1903 Dick Act and the 1916 National Defense Act, which converted the militias into a federally funded, federally commanded National Guard. The arrangement then persisted for roughly ninety years on a mandate whose object no longer existed, sustained by recitation of the militia preamble (theater_ratio climbing from 0.12 to 0.58 across the operative interval) and by a successor function: gatekeeping individual claims on behalf of state regulatory autonomy. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) flags this zombie persistence, and the theater trajectory corroborates it. The classification prevents both mislabels: a pure-coordination reading would erase the post-atrophy decades of claim absorption; a pure-extraction reading would erase the genuine founding-era federalism achievement the arrangement originally delivered. The tangled-rope claim holds both truths across the lifecycle, with omega post_atrophy_persistence_driver carrying the open question of what fraction of late-period persistence was inertia versus active utility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_right_holder_contest,
    'This constraint is one reading of the second_amendment_arms_right kernel — the collective-right reading locating the right''s holder in state militia authority. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'The disagreement is located in right-holder identity — a single variable the readings fill incompatibly (state institution / private person / citizen-agent). Judicial settlement arrived in Heller (2008) for federal law in favor of the individual reading, leaving this reading as a minority scholarly position; a full resolution would require interpretive agreement on the operative force of the prefatory clause.',
    'Under the individual-right reading the victim set contracts dramatically (regulated individuals become rights-holders), epsilon on identical prohibition measures rises sharply, and this reading''s beneficiaries lose their shield. Under the civic-republican reading protection attaches conditionally through civic-military participation, producing a hybrid beneficiary/victim structure neither sibling contains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_right_holder_contest, conceptual, 'Kernel contest over right-holder identity among three readings of one ratified text.').

omega_variable(
    protected_object_existence,
    'After the 1903 Dick Act and the 1916 National Defense Act folded the state militias into a federally funded and federally commanded National Guard, does the reading''s protected object — an independent state military force — still exist for the arrangement to protect?',
    'Institutional history of the Guard''s federal integration (funding, muster, command, deployment authority) together with doctrinal analysis of what ''state militia'' could denote after 1916.',
    'If the object no longer exists, the arrangement''s coordination function is vestigial and its persistence is sustained by successor functions (claim gatekeeping, regulatory autonomy), pushing the late-period profile toward piton-like inertia inside the tangled-rope envelope; if a meaningful state military residue persists, part of the coordination function remains live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protected_object_existence, empirical, 'Whether the arrangement''s protected object survived the federalization of the militia system.').

omega_variable(
    consolidation_era_disparate_enforcement,
    'How much of the reading''s consolidation-era operation (Presser through mid-century) functioned to sustain racially targeted state disarmament — particularly of freedmen in the South — as opposed to neutral federalism maintenance?',
    'Historical record linking state disarmament statutes and their enforcement demographics to the doctrinal citations relied upon, including Presser''s parade holding and the exclusion of freedmen from Southern militia rolls during Reconstruction.',
    'A large disparate-enforcement component raises the effective burden borne by excluded populations and supports reading the arrangement''s historical operation as more extractive than its federalism rationale admits; a small component supports the coordination-first account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consolidation_era_disparate_enforcement, empirical, 'Disparate impact of the reading''s consolidation-era enforcement on disarmed minority populations.').

omega_variable(
    post_atrophy_persistence_driver,
    'What sustained the arrangement for roughly ninety years after its founding problem died — inertial doctrinal entrenchment alone, or active utility to state regulatory autonomy and judicial economy in dismissing individual claims?',
    'Counterfactual and citation-pattern analysis: whether opinions after 1916 lean on the militia rationale or on the dismissal convenience; comparison with how courts treated other constitutional constructions whose objects had vanished.',
    'If persistence was utility-driven, the late-period arrangement is better read as maintained by the beneficiaries of its successor function; if inertial, decay dynamics dominate and the theater ratio understates how far the arrangement had become performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_atrophy_persistence_driver, conceptual, 'Relative weight of inertia versus active utility in sustaining the arrangement past its mandate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1886, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1886, second_amendment_arms_right__collective_right_reading, theater_ratio, 1886, 0.12).
narrative_ontology:measurement(seco_tr_t1920, second_amendment_arms_right__collective_right_reading, theater_ratio, 1920, 0.26).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__collective_right_reading, theater_ratio, 1939, 0.36).
narrative_ontology:measurement(seco_tr_t1960, second_amendment_arms_right__collective_right_reading, theater_ratio, 1960, 0.46).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_arms_right__collective_right_reading, theater_ratio, 1980, 0.52).
narrative_ontology:measurement(seco_tr_t1994, second_amendment_arms_right__collective_right_reading, theater_ratio, 1994, 0.58).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.71).

% Extraction over time
narrative_ontology:measurement(seco_be_t1886, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1886, 0.24).
narrative_ontology:measurement(seco_be_t1920, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1920, 0.29).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1939, 0.34).
narrative_ontology:measurement(seco_be_t1960, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1960, 0.39).
narrative_ontology:measurement(seco_be_t1980, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1980, 0.41).
narrative_ontology:measurement(seco_be_t1994, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1994, 0.43).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1886, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1886, 0.34).
narrative_ontology:measurement(seco_su_t1920, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1920, 0.44).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1939, 0.56).
narrative_ontology:measurement(seco_su_t1960, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1960, 0.63).
narrative_ontology:measurement(seco_su_t1980, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1980, 0.66).
narrative_ontology:measurement(seco_su_t1994, second_amendment_arms_right__collective_right_reading, suppression_requirement, 1994, 0.68).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__collective_right_reading, suppression_requirement, 2008, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one ratified text decomposes into three structurally distinct constraints because the readings assign the right to incompatible holders, producing different victim sets, different epsilon on identical prohibition measures, and different classifications. This reading sat upstream of the individual-right reading during 1886-2008 (its judicial dominance starved the sibling of doctrinal resources) and downstream after 2008, when Heller reversed the influence direction. Linked via affects_constraints per the family rule; the decomposition follows the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
