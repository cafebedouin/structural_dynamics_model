% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow Linking Reading: Linking Is Aggregation, Obligations Attach Only To Modification
 *   domain: legal/technological/software-governance
 *
 * SUMMARY:
 *   The colloquial question 'when does linking to GPL code trigger
 *   obligations?' decomposes, per the epsilon-invariance principle, into
 *   three structurally distinct constraints: the broad copyleft reading (any
 *   linking triggers disclosure), the interface boundary reading (clean API
 *   boundaries are non-derivative even under tight coupling), and this file's
 *   narrow linking permissive reading (linking is aggregation; only
 *   modifications to the licensed code itself trigger obligations). Each has
 *   its own epsilon, its own beneficiary/victim structure, and its own
 *   failure modes; they are linked as a constraint family via
 *   network.affects_constraints rather than forced into one story with a
 *   measurement parameter. This story instantiates the narrow reading only.
 *   Its operation: a proprietary vendor may link GPL libraries into a closed
 *   product, owing source disclosure solely for files of the licensed program
 *   it modified. The structural consequence is a wall protecting proprietary
 *   modules; users of those modules lose the source-availability guarantee
 *   the license text promised; the stewards' propagation goal is frustrated.
 *   KEY AGENTS (by structural relationship): - proprietary_software_vendors:
 *   primary beneficiary (powerful/constrained) - receives uncompensated reuse
 *   of licensed components - embedded_device_manufacturers: secondary
 *   beneficiary (powerful/constrained) - ships closed firmware around GPL
 *   components - copyleft_license_stewards: primary organized target
 *   (organized/identity_locked) - administers the license whose scope courts
 *   narrowed - gpl_contributors: diffuse targets (moderate/constrained) -
 *   unpaid labor subsidizes closed products - end_users_of_combined_works:
 *   terminal targets (powerless/trapped) - receive binaries without full
 *   source - federal_courts: agenda setter (institutional/constrained) - set
 *   the effective scope case by case - open_source_compliance_bar: analytical
 *   observer (organized/analytical) - nonparty_gpl_rights_holders: excluded
 *   voice (powerless/trapped) - bound by precedent they never litigated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.55).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.38).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow Linking Reading: Linking Is Aggregation, Obligations Attach Only To Modification").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "legal/technological/software-governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '1a77b5bf-ed72-4219-8cf2-bf5717c42d8e').
narrative_ontology:cs_kernel_codification('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', fixed_text).
narrative_ontology:cs_authority_grounding('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', lineage).
narrative_ontology:cs_interpretation_layer_present('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e').
narrative_ontology:cs_reading_relation('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', gpl_derivative_work_trigger__interface_boundary_reading, influences).
narrative_ontology:cs_axiom('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', secondary, obligations_confined_to_code_modification).
narrative_ontology:cs_axiom_status(obligations_confined_to_code_modification, holdable).
narrative_ontology:cs_axiom_grounding('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', obligations_confined_to_code_modification, conventional).
narrative_ontology:cs_reference_frame('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', statutory_derivative_work_baseline).
narrative_ontology:cs_drift_state('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', post_api_fair_use_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1a77b5bf-ed72-4219-8cf2-bf5717c42d8e', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, embedded_device_manufacturers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_license_stewards).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_combined_works).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, aggregation_not_derivation_doctrine).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, abstraction_filtration_comparison_test).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__narrow_linking_permissive_reading, statutory_derivative_work_literalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build commercial products that link against GPL-licensed libraries, compilers, and runtime components. Under the prevailing narrow interpretation they owe source disclosure only for files of the licensed program they actually modified, so the bulk of their stack stays closed while the licensed components do the heavy lifting. Leaving would mean rewriting products against permissively licensed substitutes, but the mature GPL components they depend on have no drop-in equivalents, so departure is expensive and slow.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, constrained, global).

% Ship consumer hardware - routers, televisions, appliances - whose firmware incorporates GPL components. Released products satisfy their obligations by offering source for the modified licensed files alone; the applications layered above stay closed for the life of the device. Redesigning firmware stacks around alternative components would span multiple hardware generations, so they remain in the arrangement.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, embedded_device_manufacturers, beneficiary,
    powerful, biographical, constrained, global).

% Publish and maintain the license texts and the interpretive FAQ asserting that linking brings a combined work within the license's scope. Courts and industry practice have declined to follow that assertion, narrowing what the instrument actually secures. They continue administering the license, litigating selected cases, and drafting successor versions; abandoning the propagation mission the organization exists to pursue is not a live option, so they contest from inside a structure they do not control.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_license_stewards, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyleft_license_stewards, agenda_setter).

% Individual developers who contribute code to GPL projects, often under copyright assignment or community norms. The reciprocity they understood themselves to be accepting - that downstream users of combined works receive source - is unenforceable at the linking boundary, so their unpaid work ends up inside closed products. Stopping contributions or relicensing their portions carries reputational and community-standing costs that keep most of them in place.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_contributors, payer,
    moderate, biographical, constrained, global).

% Run devices and applications that embed GPL components inside closed stacks. They receive the licensed components' source but no source for the surrounding product, cannot audit or repair what they run, and face high switching costs across locked hardware, proprietary formats, and installed bases.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_combined_works, payer,
    powerless, immediate, trapped, global).

% Decide case by case what counts as a derivative work for copyright purposes, thereby setting the effective scope of license obligations for the litigants and, through precedent, for nonparties. Stare decisis and reliance interests constrain revision; en banc review and higher-court review are the slow levers by which the line can move.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).

% Law firms and advisory practices that interpret license scope for clients on both sides, draft compliance programs, and opine on whether particular linking patterns trigger obligations. Their business follows interpretive uncertainty in whichever direction it runs.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_compliance_bar, observer,
    organized, biographical, analytical, global).

% Rights holders in GPL code whose works were linked into products in litigation they were never part of; the precedent set between other parties fixes the scope of their own licenses without their voice. Funding an independent suit to relitigate the question exceeds their means.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, nonparty_gpl_rights_holders, excluded,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws the copyright boundary for combined works: a single predictable rule telling developers which combinations of licensed and proprietary code trigger source-sharing obligations, allowing mixed ecosystems to form without case-by-case litigation risk over every act of combination.
% TRANSFER_FUNCTION: Moves the value of GPL-licensed code and its contributors' unpaid labor into proprietary products without reciprocal source disclosure; moves legal certainty to combiners; moves away from license stewards the reciprocity their license text promised, and away from end users the source-availability guarantee.
% ABSENT_VOICES: End users of combined works have no seat in doctrine formation - no standing, no collective voice, no resources. Nonparty GPL rights holders are bound by precedent set between other parties. Contributors speak only through steward organizations, and the stewards argue but do not adjudicate. The compliance bar profits from ambiguity on both sides and has no incentive to press for resolution.
% DISAPPEARANCE_RATIONALE: If the settled narrow rule vanished overnight - if a controlling court adopted the broad reading - every mixed-stack product's obligations would reopen at once: vendors would face disclosure demands across installed products, mass relicensing and forking would follow, some products would drop GPL components entirely, and the compliance market would surge. Arrangements across the entire commercial software economy depend on the current line.
% FOUNDING_PROBLEM: As mixed proprietary and freely licensed software ecosystems emerged in the late 1980s and 1990s, copyright law needed a workable definition of 'derivative work' for software combination: protect code without letting the monopoly reach every connection, so that interoperability and combination remained possible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the appellate opinions themselves (the interface-deprotecting line culminating in the API fair-use decisions) articulate the interoperability problem they were solving; copyright treatises and academic scholarship attest the boundary question remains unresolved at each new linkage technology; the stewards' own writings attest the problem exists while disputing this reading's answer to it. No party denies the founding problem's existence; the contest is over the solution.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.55 is reading-indexed over the fixed referent (the standing narrow-linking arrangement): this reading concedes the uncompensated commons-to-vendor flow its own doctrine permits while denying wrongfulness at the margin, so it authors materially lower extraction than a broad-reading story would over the identical referent; the value is stable within this reading. Suppression 0.38 is a raw structural property, unscaled: adverse precedent and litigation-risk asymmetry chill broad-claim enforcement, but alternatives (permissive substitutes, AGPL-style redrafting, clean-room reimplementation) remain lawful and real. Theater_ratio 0.30: compliance programs genuinely verify literal-file obligations while the linking question is settled by silence, and the ceremonial share grew as the narrow reading hardened into industry practice. Accessibility_collapse 0.45 reflects partially surviving alternatives; resistance 0.55 reflects the live kernel contest itself: steward litigation, GPLv3 and AGPL drafting, and scholarly opposition. The measurement series run on one shared six-point grid (t=0..40, roughly 1985-2025) with all three tracked metrics authored at every point: extractiveness rose as interface-deprotecting decisions (Sega, Lotus, culminating in the API fair-use line) let industry rely on the narrow reading; suppression_requirement fell as the doctrine settled in this reading's favor and enforcement need decayed; theater rose as compliance became ceremonial. Trajectories are monotonic; no cyclical mechanism is present.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor and manufacturer seats the arrangement computes as ordinary permission: a boundary that simply fails to reach them, experienced as low-obligation coordination. From the steward and contributor seats the same boundary operates as a broken promise enforced by courts: the reciprocity the license text described is unenforceable at exactly the point of combination. From the bench the structure is administration of a statutory term. The engine computes these divergent per-seat classifications from the structural data; the divergence between the beneficiary seats' rope-like experience and the payer seats' extraction experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the two vendor-class seats toward the beneficiary end (low d, damped or inverted effective extraction): they receive the transfer and their exit, while costly, is real. Victim declarations drive stewards, contributors, and end users toward the target end (high d, amplified extraction), with amplification strongest for end users (trapped, powerless) and stewards (identity-locked to a mission the constraint defeats). Contributors sit slightly nearer symmetric than end users because their exit (stop contributing, relicense) is constrained but not closed. Courts are neither beneficiary nor victim; the explicit override sets institutional d to 0.45, encoding a near-neutral administrator with mild systemic investment in doctrinal stability. The compliance bar derives an observer-neutral position. Larger-than-national scope modestly amplifies effective extraction on the target side, since verification of linking practices across global product fleets is harder than domestic-scale verification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a workable derivative-work boundary for software combination) is live, so no mandate-atrophy is declared and the mismatch consumer finds status=live x verdict=world_rearranges, producing no zombie flag. The classification danger runs in both directions: calling this a rope would hide the one-way value flow from contributors and users to vendors; calling it a snare would erase the genuine certainty function every legitimate combiner relies on and that courts deliberately supplied. The tangled_rope framing keeps both faces visible: real coordination (a bright-line rule enabling mixed ecosystems) and real asymmetric extraction (uncompensated commons appropriation), held together by active judicial maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_delta,
    'This constraint is one reading of the gpl_derivative_work_trigger kernel; what structurally changes if the broad_copyleft_reading prevails instead?',
    'Adoption of the broad reading by a controlling court would flip the beneficiary/victim structure: proprietary vendors and device manufacturers become obligation-bearing targets, stewards collect the reciprocity their license text promises, and this reading''s permissive wall becomes an enforced gate.',
    'Classification of the identical linking arrangements moves from permissive-wall to enforced-reciprocity; epsilon over the same referent rises sharply; the kernel family''s three stories are mutually exclusive holdings, not cumulative layers, and cross-reading comparison is valid only seat-by-seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_delta, conceptual, 'Committer structure: the sibling broad reading would invert the transfer direction over the same referent.').

omega_variable(
    statutory_vs_contractual_ground,
    'Is the narrow reading''s holding force grounded in the copyright statute''s derivative-work definition, or in license-contract interpretation that drafters could widen by agreement?',
    'Courts drawing the copyright/preemption line in license-enforcement cases; doctrinal scholarship distinguishing statutory limits from private ordering.',
    'If the ground is contractual, the wall is opt-out-able per project (stronger drafting, network-use clauses) and extraction concentrates only where stewards drafted narrowly; if statutory, the wall binds regardless of drafting and stewards have no private-ordering escape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_vs_contractual_ground, empirical, 'Whether the reading''s force is statutory ceiling or default rule.').

omega_variable(
    linkage_technology_drift,
    'Does the aggregation/derivation line survive new linkage forms: dynamic loading at scale, containers, plugin architectures, service boundaries, machine-generated glue code?',
    'Future infringement litigation and scholarly treatment of each successive linkage technology, tracing whether courts extend, distinguish, or abandon the aggregation metaphor.',
    'Technological drift could collapse the bright line toward the interface_boundary_reading or revive broad-copyleft claims, moving this reading from settled to contested and changing its enforcement requirements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linkage_technology_drift, empirical, 'Durability of the linking/aggregation boundary under technological change.').

omega_variable(
    uncompensated_flow_magnitude,
    'How much GPL-contributed value reaches proprietary products without reciprocal source disclosure under this reading?',
    'Repository-mining and binary-analysis studies measuring linking incidence, component reuse, and disclosure rates across commercial products.',
    'A large measured flow supports treating the arrangement as primarily extractive despite its coordination function; a small flow supports the coordination-first account and a lower effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncompensated_flow_magnitude, empirical, 'Quantifying the one-way value flow the reading permits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gpl__tr_t8, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(gpl__tr_t16, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(gpl__tr_t32, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(gpl__tr_t40, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t8, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(gpl__be_t16, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(gpl__be_t32, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement(gpl__be_t40, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gpl__su_t8, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(gpl__su_t16, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(gpl__su_t32, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 32, 0.43).
narrative_ontology:measurement(gpl__su_t40, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the label 'GPL derivative work trigger' decomposes into three stories per the epsilon-invariance principle. The broad_copyleft_reading is the license text's self-description (upstream: the stewards' authoritative FAQ and the text's own preamble supply its premises); this narrow reading is the downstream judicial-and-industry settlement that declined to follow the upstream claim; the interface_boundary_reading is a intermediate doctrinal position governing coupling without linking. Each story carries a single stable epsilon over the shared referent (the standing linking arrangements); the upstream story's claims are cited as evidence within the downstream contest, which is why the family edges run upstream-to-downstream. Cross-reading epsilon comparison is seat-indexed, not topic-indexed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
