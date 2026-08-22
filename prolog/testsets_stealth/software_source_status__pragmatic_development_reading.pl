% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Open-Source-as-Superior-Methodology Norm (Pragmatic Development Reading)
 *   domain: technological/political-economic/intellectual-property
 *
 * SUMMARY:
 *   Since 1998, the governing norm of the software ecosystem has been the
 *   pragmatic development reading: source openness is adopted or withheld per
 *   project as an engineering decision, justified by peer review, defect
 *   detection, and innovation velocity, with proprietary arrangements
 *   remaining fully legitimate and permissive licensing acceptable. This norm
 *   genuinely solved a real coordination problem — it gave commercial actors
 *   a non-ideological entry into collaborative development and assembled the
 *   shared infrastructure of the cloud, mobile, and machine-learning eras. It
 *   also channels value asymmetrically: unpaid maintainer labor and the
 *   reputational capital of 'open' flow toward large platform firms, while
 *   the ethical tradition that built the commons is subordinated to
 *   efficiency talk. FAMILY NOTE: this file is one of four epsilon-invariant
 *   readings of the software_source_status kernel. The label 'open vs. closed
 *   source' conflates structurally distinct claims; each reading is a
 *   separate story with its own epsilon over the SAME standing arrangement
 *   (the mixed permissive/commercial ecosystem). The
 *   freedom_imperative_reading authors high epsilon over that arrangement;
 *   the property_rights_reading authors near-floor epsilon; this reading
 *   authors intermediate epsilon (0.64). Files are linked via
 *   network.affects_constraints. CLAIM/METRIC INDEPENDENCE: claimed_type
 *   tangled_rope is authored from structural belief (real coordination
 *   function plus asymmetric extraction through the same structure); the
 *   metrics are authored independently as descriptive truth; the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - - large_platform_firms: Primary beneficiary and de facto agenda-setter (institutional/arbitrage) — consumes the commons, steers governance, can relicense or fork at will
 *   - - volunteer_maintainers: Primary target (powerless/trapped) — unpaid labor bearing maintenance burden, security liability, and burnout
 *   - - free_software_movement: Subordinated normative competitor (organized/identity_locked) — bears discursive suppression of the ethical frame
 *   - - open_source_foundations: Administrative intermediary (institutional/constrained) — runs governance, absorbs drift through interpretation, funded by corporate dues
 *   - - enterprise_downstream_consumers: Secondary beneficiary (powerful/mobile) — consumes components without reciprocal obligation
 *   - - end_users_and_dependents: Excluded party (powerless/trapped) — absorbs rug-pull relicensing shocks with no seat
 *   - - software_engineering_researchers: Analytical observer (analytical/analytical) — produces the evidence base the instrumental claim stands or falls on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.64).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.51).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.51).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Open-Source-as-Superior-Methodology Norm (Pragmatic Development Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "technological/political-economic/intellectual-property").

domain_priors:requires_active_enforcement(software_source_status__pragmatic_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'd354171c-e3c2-4bd7-8fcb-4643f4337844').
narrative_ontology:cs_kernel_codification('d354171c-e3c2-4bd7-8fcb-4643f4337844', distributed).
narrative_ontology:cs_authority_grounding('d354171c-e3c2-4bd7-8fcb-4643f4337844', expertise).
narrative_ontology:cs_interpretation_layer_present('d354171c-e3c2-4bd7-8fcb-4643f4337844').
narrative_ontology:cs_reading_relation('d354171c-e3c2-4bd7-8fcb-4643f4337844', software_source_status__freedom_imperative_reading, influences).
narrative_ontology:cs_reading_relation('d354171c-e3c2-4bd7-8fcb-4643f4337844', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d354171c-e3c2-4bd7-8fcb-4643f4337844', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d354171c-e3c2-4bd7-8fcb-4643f4337844', foundational, openness_instrumental_to_quality).
narrative_ontology:cs_axiom_status(openness_instrumental_to_quality, holdable).
narrative_ontology:cs_axiom_grounding('d354171c-e3c2-4bd7-8fcb-4643f4337844', openness_instrumental_to_quality, empirically_contingent).
narrative_ontology:cs_axiom('d354171c-e3c2-4bd7-8fcb-4643f4337844', foundational, proprietary_licensing_legitimate).
narrative_ontology:cs_axiom_status(proprietary_licensing_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d354171c-e3c2-4bd7-8fcb-4643f4337844', proprietary_licensing_legitimate, conventional).
narrative_ontology:cs_reference_frame('d354171c-e3c2-4bd7-8fcb-4643f4337844', instrumental_methodology_pluralism).
narrative_ontology:cs_drift_state('d354171c-e3c2-4bd7-8fcb-4643f4337844', contemporary_post_open_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d354171c-e3c2-4bd7-8fcb-4643f4337844', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, large_platform_firms).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, enterprise_downstream_consumers).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, open_source_foundations).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, volunteer_maintainers).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, free_software_movement).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, cathedral_bazaar_efficiency_thesis).
narrative_ontology:constraint_vindicates(software_source_status__pragmatic_development_reading, linus_law_eyeballs_shallow_bugs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the largest cloud, mobile, and AI products built on collaboratively developed components. Run open-source program offices, employ prominent maintainers, hold foundation board seats, and fund conferences. They decide per project whether to open or close code, can relicense or fork at will, and can withdraw support without losing their own products. Their participation decisions effectively set the terms the other participants negotiate within.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, large_platform_firms, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, large_platform_firms, agenda_setter).

% Hold trademarks, run governance and trademark-policy processes, host contributor agreements, and convene the committees that decide what 'open' covers. Funded chiefly by corporate membership dues. When practice drifts — source-available products, AI training disputes — they issue interpretive guidance that absorbs the drift without reopening the underlying question of what source access is for.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, open_source_foundations, beneficiary).

% Keep widely depended-upon packages running, mostly unpaid, alongside day jobs. Once a package becomes load-bearing for strangers' production systems, departing imposes harm on users and reputational cost on the maintainer, so departure is effectively unavailable. They absorb security liability, burnout, and entitlement pressure from commercial users; they were the population exposed by the 2024 xz-utils compromise.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, volunteer_maintainers, payer,
    powerless, biographical, trapped, global).

% Organizations and veterans of the ethical tradition that predates the methodology framing. They attend the same conferences and sit in the same standards bodies, but their arguments are received only when translated into efficiency terms. Their defining commitment — that source access is a matter of justice rather than convenience — has lost definitional authority to the methodology framing. Leaving the conversation would mean abandoning the commitment that constitutes them.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, free_software_movement, payer,
    organized, civilizational, identity_locked, global).

% Build commercial products on open components selected through competitive procurement. They contribute selectively where contribution buys influence and consume otherwise, and they can switch suppliers or bring development in-house when terms turn unfavorable. They carry little of the maintenance burden their products rest on.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, enterprise_downstream_consumers, beneficiary,
    powerful, biographical, mobile, global).

% Run software whose licenses can change unilaterally; the 2023-2024 relicensing wave reached them as sudden cost and compliance shocks. They have no seat in methodology debates or foundation governance and typically learn the new terms only after their dependence is sunk.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, end_users_and_dependents, excluded,
    powerless, immediate, trapped, global).

% Produce the defect-density, maintenance-economics, and governance studies that the instrumental case for openness stands or falls on. Affiliated with universities and independent labs; no revenue depends on which framing wins.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, large_platform_firms).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives engineering organizations and volunteer communities a shared, evidence-based criterion for choosing per project between collaborative and restricted development, plus a common vocabulary of licenses, foundations, and contribution norms that lets commercial and volunteer actors collaborate without first resolving the ethical question of what source access is for.
% TRANSFER_FUNCTION: Moves unpaid maintenance labor and reputational capital ('open') from volunteer communities toward commercial actors who consume the commons; moves discursive priority from justice-based claims about source access to efficiency-based claims; moves governance authority toward corporate-funded foundations.
% ABSENT_VOICES: End users and downstream dependents — the population hit by rug-pull relicensing — have no seat in methodology debates or foundation governance. Volunteer maintainers are present but without agenda power. The freedom-imperative voice participates but is heard only after translation into efficiency terms.
% DISAPPEARANCE_RATIONALE: Overnight removal would collapse the commercial-volunteer collaboration equilibrium assembled since 1998: firms would retreat to proprietary stacks or outright buyouts, volunteer projects would lose the contribution pipelines and funding that sustain them, and the shared infrastructure of cloud, mobile, and machine-learning systems would reorganize around either copyleft exclusivity or closed procurement — a rearrangement measured in decades.
% FOUNDING_PROBLEM: The late-1990s standoff: free software's ethical absolutism made commercial participation politically impossible, while proprietary fragmentation kept development costs high and duplicated work. The pragmatic reading was built to give businesses a non-ideological entry into collaborative development.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the OSI founders' own 1998 positioning materials (explicitly targeting business audiences) and the FSF's contemporaneous critique both attest that the founding problem was the commercial-adoption barrier. Academic histories of the OSI split and maintainer-economics surveys (the Ford Foundation/Linux Foundation census) attest the cost side from independent seats. No source outside the disputing camps attests the problem is still live — the beneficiaries cite new frontiers (AI training, sustainability) while critics hold the original bridge long since built — which is why status is contested rather than live.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.64: the standing arrangement transfers maintenance labor, reputational capital, and discursive priority from volunteers and the ethical tradition toward commercial actors; the transfer is substantial but not coercive — hence below snare-range. Suppression 0.51: suppression here is discursive and institutional (funding, hiring, venue gatekeeping, the requirement that justice claims arrive translated as efficiency claims), not physical; it is authored as a raw structural property and is NOT scaled by power or scope — the engine owns any scaling of extractiveness only. Theater_ratio 0.52: open-washing, sponsorship-without-governance, and 'community-driven' branding now rival the functional core, crossing the Goodhart threshold; the methodology function remains real, so the ratio sits just above 0.5 rather than far above it. Accessibility_collapse 0.28: the norm ranks development models rather than eliminating them — proprietary, copyleft, and source-available practice all remain fully legible and exercised, so alternatives survive understanding of the constraint. Resistance 0.55: sustained critique from the free-software tradition, relicensing backlashes, and maintainer collective actions (coordinated license changes, maintenance strikes) meet the norm continuously without displacing it. MEASUREMENTS: one shared eight-point grid (1998-2026) for all three tracked metrics; suppression_requirement is authored because enforcement-capacity change IS the traced dynamic — gatekeeping machinery built steadily from 1998 to roughly 2018, then plateaued and softened slightly as the agenda-setting firms themselves began defecting from the norm via relicensing (enforcement decay at the top while extraction continued to accumulate). The monotonically rising base_extractiveness series is the extraction-accumulation signature; the advisory abductive trigger should fire, and that is intended data, not noise. The 2026 points are authored projections from observed 2022-2024 events (the relicensing wave, the xz-utils exposure of unpaid-labor fragility) and are marked projected. Gains demonstrably accrue to the large-platform seat, which is why gain_flow names it; fixing_cost is prohibitive because any unilateral reciprocity move (generous maintainer funding, binding contribution obligations) erodes the mover's competitive position relative to rivals who do not move — correction requires coordinated action across competitors, which the structure itself discourages.
 *
 * PERSPECTIVAL GAP:
 *   From the platform-firm seat the arrangement reads as meritocratic efficiency: we fund foundations, employ maintainers, and open what serves quality. From the volunteer-maintainer seat the same structure reads as uncompensated obligation: commercial users consume the output, file the entitlements, and vanish at relicensing time. From the free-software seat it reads as colonization of an ethical commons by a frame that keeps the artifacts and discards the reasons. Same-level lateral divergence: enterprise downstream consumers and volunteer maintainers occupy nominally similar 'participant' positions with opposite exits — the consumer holds mobile/arbitrage exit (switch suppliers, insource), the maintainer holds trapped exit (load-bearing obligation, unsellable departure), so equal nominal standing produces opposite experienced constraints. Coalition potential for the powerless seat runs through maintainer collective action; it has fired locally (package-license protests, maintenance strikes) but has not sustained organizationally, which is why resistance sits at 0.55 rather than higher. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   large_platform_firms: declared beneficiary with arbitrage-grade exit — derivation places d near the beneficiary end (~0.05-0.15); effective extraction inverts toward subsidy. enterprise_downstream_consumers: beneficiary, mobile exit — similarly low d. open_source_foundations: agenda-setter with declared secondary beneficiary position and constrained exit — low-to-symmetric d; they collect relevance and funding without bearing the costs they administer. volunteer_maintainers: declared victim, powerless, trapped — d near the full-target end (~0.85-0.95); trapped exit amplifies effective extraction. free_software_movement: declared victim, organized power but identity_locked exit — high d; identity lock keeps them at the table absorbing costs they cannot walk away from. end_users_and_dependents: excluded, no beneficiary/victim declaration — canonical fallback applies; their exclusion is itself part of the structure. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct relationships, and the guidance reserves overrides for cases the derivation gets wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Calling this a pure rope ignores the appropriation asymmetry riding the coordination function — maintainer burnout, value capture without reciprocity, the subordination of the ethical frame. Calling it a snare ignores the genuine, still-operating coordination achievement: the 1998 bridge is why collaborative development powers modern infrastructure at all. On mandatrophy: the founding problem (the commercial-adoption barrier) is substantially solved — collaboration is now the default — but the frame acquired new governing functions (AI-training disputes, sustainability allocation) before the old mandate died, so the mandate has transformed rather than outlived its function; mandatrophy is therefore not declared. The drift risk to watch is piton-shaped: theater_ratio at 0.52 and rising means the quality rationale is progressively replaced by marketing performance; if the instrumental premise loses empirical support (see the quality_premise omega) while the rhetoric persists, this story dates its own transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the pragmatic_development_reading of the software_source_status kernel; how would classification shift if a sibling reading governed the same ecosystem?',
    'Compare compiled classifications across the four sibling stories (freedom_imperative, property_rights, utilitarian_hybrid); divergence localizes which structural element (victim set, enforcement surface, scope) each reading reweights.',
    'Under the freedom_imperative_reading the same ecosystem''s epsilon rises sharply (proprietary layers become injustices); under the property_rights_reading epsilon falls toward a coordination-cost baseline. This story''s intermediate epsilon is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a shared kernel; sibling stories carry the structural deltas.').

omega_variable(
    reciprocity_separability,
    'Is the measured extraction inherent to the instrumental framing of source access, or separable from it — correctable by reciprocity mechanisms (funded-maintainer mandates, governance rebalancing) that the frame itself can host?',
    'Natural experiments where reciprocity mechanisms bind without abandoning instrumental framing: foundation sustainability funding programs, regulatory maintenance-cost allocation. If extraction metrics fall where such mechanisms operate, the extraction is separable.',
    'Separable: the arrangement trends toward pure coordination as governance matures. Inherent: entrenched hybrid with drift risk toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_separability, empirical, 'Whether the appropriation asymmetry is intrinsic to the frame or an artifact of current corporate dominance.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of freedom-framed argument structural (funding, hiring, and venue gatekeeping) or internalized (professional norms that pre-translate ethical claims into efficiency claims)?',
    'Post-exit suppression trajectory: compare freedom-framed discourse in venues without corporate sponsorship against sponsored venues; persistence of self-translation habits among engineers who move to unsponsored settings indicates internalization.',
    'If internalized, effective suppression exceeds the structural measure and persists after institutional gates fall; the scalar suppression value conflates two mechanisms with different lifetimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of the subordinated ethical frame.').

omega_variable(
    quality_premise_empirical_status,
    'Does open development actually outperform closed development on quality outcomes, or is the supporting evidence confounded by selection effects (the most-resourced, most-visible projects are the open ones)?',
    'Matched-pair studies controlling for project salience, team size, and domain; preregistered security-defect density comparisons.',
    'If the instrumental premise fails empirically, this reading''s foundational axiom (empirically contingent) is overridden and authority migrates toward the utilitarian_hybrid or property_rights siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quality_premise_empirical_status, empirical, 'Empirical status of the reading''s foundational quality claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 1998, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t1998, software_source_status__pragmatic_development_reading, theater_ratio, 1998, 0.14).
narrative_ontology:measurement_basis(soft_tr_t1998, observed).
narrative_ontology:measurement(soft_tr_t2002, software_source_status__pragmatic_development_reading, theater_ratio, 2002, 0.19).
narrative_ontology:measurement_basis(soft_tr_t2002, observed).
narrative_ontology:measurement(soft_tr_t2006, software_source_status__pragmatic_development_reading, theater_ratio, 2006, 0.25).
narrative_ontology:measurement_basis(soft_tr_t2006, observed).
narrative_ontology:measurement(soft_tr_t2010, software_source_status__pragmatic_development_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement_basis(soft_tr_t2010, observed).
narrative_ontology:measurement(soft_tr_t2014, software_source_status__pragmatic_development_reading, theater_ratio, 2014, 0.37).
narrative_ontology:measurement_basis(soft_tr_t2014, observed).
narrative_ontology:measurement(soft_tr_t2018, software_source_status__pragmatic_development_reading, theater_ratio, 2018, 0.43).
narrative_ontology:measurement_basis(soft_tr_t2018, observed).
narrative_ontology:measurement(soft_tr_t2022, software_source_status__pragmatic_development_reading, theater_ratio, 2022, 0.48).
narrative_ontology:measurement_basis(soft_tr_t2022, observed).
narrative_ontology:measurement(soft_tr_t2026, software_source_status__pragmatic_development_reading, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(soft_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(soft_be_t1998, software_source_status__pragmatic_development_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement_basis(soft_be_t1998, observed).
narrative_ontology:measurement(soft_be_t2002, software_source_status__pragmatic_development_reading, base_extractiveness, 2002, 0.34).
narrative_ontology:measurement_basis(soft_be_t2002, observed).
narrative_ontology:measurement(soft_be_t2006, software_source_status__pragmatic_development_reading, base_extractiveness, 2006, 0.4).
narrative_ontology:measurement_basis(soft_be_t2006, observed).
narrative_ontology:measurement(soft_be_t2010, software_source_status__pragmatic_development_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement_basis(soft_be_t2010, observed).
narrative_ontology:measurement(soft_be_t2014, software_source_status__pragmatic_development_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement_basis(soft_be_t2014, observed).
narrative_ontology:measurement(soft_be_t2018, software_source_status__pragmatic_development_reading, base_extractiveness, 2018, 0.57).
narrative_ontology:measurement_basis(soft_be_t2018, observed).
narrative_ontology:measurement(soft_be_t2022, software_source_status__pragmatic_development_reading, base_extractiveness, 2022, 0.61).
narrative_ontology:measurement_basis(soft_be_t2022, observed).
narrative_ontology:measurement(soft_be_t2026, software_source_status__pragmatic_development_reading, base_extractiveness, 2026, 0.64).
narrative_ontology:measurement_basis(soft_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t1998, software_source_status__pragmatic_development_reading, suppression_requirement, 1998, 0.25).
narrative_ontology:measurement_basis(soft_su_t1998, observed).
narrative_ontology:measurement(soft_su_t2002, software_source_status__pragmatic_development_reading, suppression_requirement, 2002, 0.33).
narrative_ontology:measurement_basis(soft_su_t2002, observed).
narrative_ontology:measurement(soft_su_t2006, software_source_status__pragmatic_development_reading, suppression_requirement, 2006, 0.41).
narrative_ontology:measurement_basis(soft_su_t2006, observed).
narrative_ontology:measurement(soft_su_t2010, software_source_status__pragmatic_development_reading, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement_basis(soft_su_t2010, observed).
narrative_ontology:measurement(soft_su_t2014, software_source_status__pragmatic_development_reading, suppression_requirement, 2014, 0.51).
narrative_ontology:measurement_basis(soft_su_t2014, observed).
narrative_ontology:measurement(soft_su_t2018, software_source_status__pragmatic_development_reading, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement_basis(soft_su_t2018, observed).
narrative_ontology:measurement(soft_su_t2022, software_source_status__pragmatic_development_reading, suppression_requirement, 2022, 0.54).
narrative_ontology:measurement_basis(soft_su_t2022, observed).
narrative_ontology:measurement(soft_su_t2026, software_source_status__pragmatic_development_reading, suppression_requirement, 2026, 0.51).
narrative_ontology:measurement_basis(soft_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, resource_allocation).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'open source vs. proprietary software' into four epsilon-invariant readings of the software_source_status kernel. The label conflates distinct claims: an ethical requirement (freedom reading), a methodology preference (this file), a property entitlement (property reading), and a welfare-maximization rule (hybrid reading). Each story carries its own epsilon over the same standing arrangement — the mixed permissive/commercial ecosystem — and its own beneficiary/victim structure. This upstream reading (highest adoption, most established) structurally influences the freedom_imperative sibling by changing its operating environment; all four files cross-link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
