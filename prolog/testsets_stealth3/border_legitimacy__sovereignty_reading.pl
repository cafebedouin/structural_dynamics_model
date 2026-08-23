% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Border Legitimacy — Sovereignty Reading: The Territorial Right to Exclude
 *   domain: political philosophy / migration studies / international law
 *
 * SUMMARY:
 *   This story instantiates the sovereignty reading of border legitimacy. The
 *   standing arrangement under contest — the referent of ε — is the
 *   state-system practice of territorial admission control: passport and visa
 *   regimes, asylum procedures, border patrols, detention, removal, carrier
 *   sanctions, and externalized enforcement. The reading assesses that
 *   arrangement by its own lights: exclusion is a legitimate exercise of
 *   sovereign authority, not a presumptive wrong, and the legitimacy claim is
 *   carried in the axioms rather than in the type claim. ε is reading-indexed
 *   over the fixed referent: this reading concedes the arrangement's costs
 *   are real and asymmetric even while holding them justified, so it authors
 *   a high but not maximal ε — lower than the freedom_of_movement sibling
 *   would author over the same arrangement, higher than a pure-coordination
 *   account would concede. The structural call from this seat is
 *   tangled_rope: a genuine coordination function (bounding the demos so it
 *   can govern itself and sustain shared institutions) operating with massive
 *   asymmetric extraction (life prospects allocated by birthplace) under
 *   active enforcement. Temporal series run 1945–2025 on one shared grid (one
 *   unit = one year).
 *
 * KEY AGENTS:
 *   - sovereign_state_executives: agenda setter (institutional/arbitrage) — sets admission policy, answers only to domestic electorates
 *   - citizen_political_communities: primary beneficiary (organized/constrained) — collects the membership rents; taxed to fund enforcement
 *   - excluded_would_be_migrants: primary target (powerless/trapped) — life plan priced by birthplace; no legal path, no vote, no standing
 *   - rejected_asylum_seekers: primary target (powerless/trapped) — non-refoulement on paper, access to a hearing controlled by enforcement
 *   - migrants_in_transit_states: primary target (powerless/trapped) — stranded in buffer states under deals they never joined
 *   - immigration_enforcement_agencies: secondary beneficiary and secondary agenda-setter (institutional/arbitrage) — budgets and powers scale with enforcement
 *   - border_security_contractors: secondary beneficiary (organized/mobile) — enforcement spending is revenue; sells across jurisdictions
 *   - employers_of_deportable_labor: secondary beneficiary (powerful/arbitrage) — precarious workforce suppresses wage demands
 *   - human_smuggling_networks: secondary beneficiary (organized/mobile) — the restriction is their market-maker
 *   - incumbent_labor_insiders: dual-positioned beneficiary/payer (moderate/constrained) — wage protection and undercutting arrive through the same border
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — documents pushbacks and litigates; holds no admission power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.71).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.78).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Border Legitimacy — Sovereignty Reading: The Territorial Right to Exclude").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political philosophy / migration studies / international law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '9febd292-5bd1-4859-9387-d8e5cea083b6').
narrative_ontology:cs_kernel_codification('9febd292-5bd1-4859-9387-d8e5cea083b6', formalized).
narrative_ontology:cs_authority_grounding('9febd292-5bd1-4859-9387-d8e5cea083b6', practice).
narrative_ontology:cs_interpretation_layer_present('9febd292-5bd1-4859-9387-d8e5cea083b6').
narrative_ontology:cs_reading_relation('9febd292-5bd1-4859-9387-d8e5cea083b6', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_reading_relation('9febd292-5bd1-4859-9387-d8e5cea083b6', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('9febd292-5bd1-4859-9387-d8e5cea083b6', foundational, political_community_self_determination_grounds_exclusion).
narrative_ontology:cs_axiom_status(political_community_self_determination_grounds_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('9febd292-5bd1-4859-9387-d8e5cea083b6', political_community_self_determination_grounds_exclusion, deontological).
narrative_ontology:cs_axiom('9febd292-5bd1-4859-9387-d8e5cea083b6', secondary, territorial_sovereignty_confers_plenary_admission_authority).
narrative_ontology:cs_axiom_status(territorial_sovereignty_confers_plenary_admission_authority, holdable).
narrative_ontology:cs_axiom_grounding('9febd292-5bd1-4859-9387-d8e5cea083b6', territorial_sovereignty_confers_plenary_admission_authority, conventional).
narrative_ontology:cs_reference_frame('9febd292-5bd1-4859-9387-d8e5cea083b6', westphalian_plenary_admission_authority).
narrative_ontology:cs_drift_state('9febd292-5bd1-4859-9387-d8e5cea083b6', contemporary_human_rights_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9febd292-5bd1-4859-9387-d8e5cea083b6', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizen_political_communities).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, immigration_enforcement_agencies).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, border_security_contractors).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, employers_of_deportable_labor).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, human_smuggling_networks).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, incumbent_labor_insiders).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_would_be_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, rejected_asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, migrants_in_transit_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, incumbent_labor_insiders).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, state_right_to_exclude).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, collective_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set admission policy through legislatures and executive action: who may enter, on what visa categories, under what asylum procedure, and with what enforcement intensity. They negotiate readmission and externalization agreements with neighboring states and can widen or narrow legal channels by statute. Their authority over the border is the thing the arrangement exists to secure; they answer to domestic electorates, not to the people their rules exclude.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, sovereign_state_executives, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate patrols, detention networks, asylum adjudication, and removal logistics. Each enforcement escalation enlarges their budgets, staffing, and statutory powers, and they shape practice through prosecutorial discretion, detention standards, and interdiction tactics. Their institutional continuity depends on the enforcement mission persisting; they also exercise real agenda-setting power over how admission rules bite in individual cases.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, immigration_enforcement_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, immigration_enforcement_agencies, agenda_setter).

% Sell surveillance towers, biometric databases, barriers, drones, and detention facility management to immigration agencies across many countries. Enforcement spending is their revenue; a tightening regime expands their market, and they can shift sales between jurisdictions and into adjacent domestic-security markets if any single market softens.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, border_security_contractors, beneficiary,
    organized, immediate, mobile, global).

% The demos whose boundaries the arrangement maintains. They receive protected labor-market positions, bounded welfare obligations, and the capacity to govern themselves without requiring consent from non-members. They also pay for the enforcement apparatus through taxation, and some bear intimate costs — family members excluded by the same rules that protect them. Their exit is emigration, which most never take; their influence runs through voting, which non-members cannot do.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizen_political_communities, beneficiary,
    organized, generational, constrained, national).

% Workers in sectors where admission control restrains labor competition; the wage and bargaining effects of restricted entry flow to them. At the same time, enforcement produces a deportable underclass employed below standards in agriculture, construction, and care work, which undercuts the same workers the rules protect. They cannot opt out of either side of this: the protection and the undercutting arrive through the same border.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, incumbent_labor_insiders, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, incumbent_labor_insiders, payer).

% Agricultural, construction, hospitality, and care employers hire from a workforce whose legal precariousness suppresses wage demands and complaints. Employer-sanctions enforcement is sporadic enough that the sanction regime functions as a price on informality rather than a bar on hiring; if enforcement tightens in one sector or jurisdiction, they shift recruitment channels or locations.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, employers_of_deportable_labor, beneficiary,
    powerful, immediate, arbitrage, national).

% Sell passage to people with no legal channel: the restriction itself is their market. Route prices rise with every fortification, and enforcement displacement moves their business across routes and brokers. They exit any given route freely when it closes and reopen elsewhere.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, human_smuggling_networks, beneficiary,
    organized, immediate, mobile, continental).

% People who would move for work, family, or safety but hold no visa category that admits them. The arrangement prices their entire plan of life by birthplace: they cannot apply for most destinations, cannot appeal the denial of a category that does not exist, and cannot vote anywhere their exclusion is decided. Their options are staying, risking irregular routes through smuggling markets, or waiting years for a lottery that admits a fraction of one percent of applicants.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_would_be_migrants, payer,
    powerless, biographical, trapped, global).

% People fleeing persecution who reach or approach a border and are turned away — by interdiction at sea, safe-third-country rules, pushbacks, or procedures designed for denial. The Refugee Convention guarantees non-refoulement on paper; externalized processing and expanded safe-country lists decide the guarantee's reach in practice. Their legal claim exists; their access to a hearing that could honor it is what the enforcement regime controls.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, rejected_asylum_seekers, payer,
    powerless, biographical, trapped, global).

% People stranded for months or years in buffer states — Mexico, Turkey, Libya, Tunisia, Indonesia — under readmission and externalization deals they were never party to. They cannot enter the destination, cannot safely return, and live under transit-state authorities that are paid and equipped by destination states to hold them in place.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, migrants_in_transit_states, payer,
    powerless, biographical, trapped, regional).

% UNHCR, treaty bodies, and regional courts assess border practices against non-refoulement guarantees, collective-expulsion bans, and access-to-asylum requirements. They document pushbacks, litigate test cases, and publish findings that states may accept, ignore, or externalize around; they hold no admission power of their own.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, citizen_political_communities).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bounding political membership: determining who may join the community so that a demos can govern itself, sustain shared welfare and labor institutions, and make binding collective decisions without requiring the consent of everyone affected worldwide.
% TRANSFER_FUNCTION: Moves access — to territory, labor markets, safety, family reunification, and life prospects — from excluded non-members to the admitting political community; moves tax revenue to enforcement agencies and security contractors; and, through the restriction itself, generates the smuggling market's revenue.
% ABSENT_VOICES: The excluded themselves. Non-members have no vote, no standing, and no seat in any legislature or agency that sets admission policy; rejected asylum seekers appear only as case files in procedures the destination state controls; the transit-stranded were never party to the externalization deals that hold them. Their objection — that the arrangement allocates life chances by birthplace — is voiced only vicariously, by advocacy organizations and treaty bodies with documentation and litigation power but no decision power.
% DISAPPEARANCE_RATIONALE: If border authority vanished overnight, the world would reorganize: labor markets would absorb new inflows and re-price; welfare states financed on closed membership would need renegotiated financing; the enforcement apparatus and its supplier industries would lose their function; smuggling markets would collapse; millions of currently excluded people would move; and the boundary of every democratic demos — the set of people whose consent its government needs — would become a live question in every state.
% FOUNDING_PROBLEM: How to bound a political community so it can govern itself: the Westphalian consolidation of territorial states, then mass democratic politics and the twentieth-century welfare state, each requiring a determinate membership — who is entitled to decide, who is protected by shared institutions, whose labor competes — in a world of radically unequal life prospects distributed by birthplace.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: open-borders theorists pressing the freedom-of-movement reading engage the self-determination problem on the merits rather than denying it exists; comparative political economy documents the membership-welfare linkage; and no state — including those rhetorically committed to freer movement — has dissolved its admission authority. What the sibling readings and the excluded themselves dispute is whether the problem justifies this arrangement's cost allocation, not whether the problem is real.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.71 is reading-indexed: this reading holds the exclusion arrangement legitimate, and a lower ε would flatter that judgment; but the descriptive transfer is enormous — access to labor markets, safety, family life, and life prospects is allocated by birthplace, the excluded hold no vote or standing anywhere the rules are set, and much of the denied value is destroyed rather than received anywhere. Suppression 0.78 is a raw structural fact, unscaled by power or scope: the arrangement persists through physical enforcement — interdiction, detention, removal, carrier sanctions, externalized processing — not through participant preference. Theater 0.4: the enforcement function is real, but a rising share is symbolic — barriers that redirect rather than stop, performative sovereignty politics, security theater that does not track flows. Accessibility_collapse 0.45: alternatives (open borders, regional free movement, expanded humanitarian and labor channels) remain live in theory and partially instantiated in practice (EU internal movement), so the constraint does not collapse its alternatives. Resistance 0.58: advocacy, sanctuary practices, litigation, and smuggling markets meet the arrangement continuously without threatening its core. Claim/metric independence: claimed_type tangled_rope states the structural call from this seat — genuine coordination function, asymmetric extraction, active enforcement; the reading's legitimacy claim lives in the axioms, not in the type. The temporal series share one grid: extractiveness dips at mid-century guestworker recruitment and quota liberalization, then rises monotonically with re-closure, securitization, and externalization; theater climbs with the symbolic turn; suppression tracks the enforcement build-up with a slight mid-century relaxation.
 *
 * PERSPECTIVAL GAP:
 *   From the payer seats (powerless, trapped), the same structure computes as enforced extraction with no exit and no voice. From the citizen-beneficiary seat (organized, constrained exit), it computes as a mostly-coordination arrangement it democratically endorses and pays for. From the agenda-setter seat (institutional, arbitrage), it computes as the exercise of its defining authority. The engine derives these divergences from the authored power/exit/role data; this story does not adjudicate them. The reading itself is a seat: the freedom_of_movement sibling, over the same referent, would compute the arrangement as suppression of a right, with ε near the top of the scale.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive d toward the subsidy end: citizen_political_communities (primary rent recipient, though taxed and bound — not a pure beneficiary), immigration_enforcement_agencies and border_security_contractors (budget and market collectors with arbitrage/mobile exit — nearest the beneficiary end among insiders), employers_of_deportable_labor (compliance-labor rent), human_smuggling_networks (the restriction is their market-maker). Victim declarations drive d toward the full-target end: excluded_would_be_migrants, rejected_asylum_seekers, and migrants_in_transit_states — all powerless with trapped exit, which sits them at the amplified end of effective extraction. incumbent_labor_insiders is declared dual (beneficiary with payer secondary): the derivation should not read them as a pure subsidy recipient. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is scaled by directionality and the scope mix — enforcement scope is national, the victim scope is global. Receipt surface: gain_flow names citizen_political_communities because the bulk of the exclusion's value — protected labor-market position, bounded welfare obligation, self-determination — accrues to the member body as a diffuse but real rent, while the concentrated fiscal stream to agencies and contractors is secondary capture; much of the denied value is destroyed rather than received, which is why no single insider seat captures the majority of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two mislabelings. Reading the arrangement as pure coordination — the sovereignty reading's own self-presentation — would erase the victim set; the type gate forces the story to name who is coordinated and who pays through the same structure. Reading it as pure extraction — the implication the freedom_of_movement sibling presses — would erase the genuine self-determination function that even open-borders theorists engage rather than deny. The founding problem (bounding the demos amid global inequality of life prospects) is live and corroborated from outside the beneficiary set, so the arrangement is not a zombie: its extraction is contested within a live function, which is the tangled_rope signature, not mandatrophy. The drift risk runs the other direction: if the founding problem ever died — global inequality collapse, or welfare decoupled from membership — the arrangement would persist by inertia, and the theater_ratio series (0.18 rising to 0.40 and climbing) is the leading indicator of that piton drift. The enforcement subsystem may pitonize first even while the core function lives: symbolic enforcement that redirects rather than stops is already two-fifths of observed activity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the sovereignty_reading of the border_legitimacy kernel; the freedom_of_movement and humanitarian_obligation siblings instantiate different constraints over the same referent. Which structural elements does the reading choice actually move — ε, the victim set, or the computed classification itself?',
    'Cross-reading comparison within the kernel family: compile all three sibling stories and compare authored ε, victim sets, and computed per-seat classifications over the identical referent (the standing exclusion arrangement).',
    'If the humanitarian reading governed, the victim set narrows to rejected_asylum_seekers and effective extraction falls; if the freedom_of_movement reading governed, ε approaches the sibling''s authored value and the enforcement machinery computes as rights-suppression rather than boundary administration. This story''s classification is valid only under its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings; classification is reading-indexed over a shared referent.').

omega_variable(
    self_determination_cost_justifiability,
    'Does the coordination function — bounding the demos for self-government and shared institutions — justify the arrangement''s cost allocation, or could membership-bounding be achieved at materially lower human cost (expanded labor channels with bounded welfare access, larger humanitarian quotas), making the excess pure extraction?',
    'Comparative institutional analysis: jurisdictions and historical episodes that bounded membership with materially wider legal channels; natural experiments from channel expansions tracking whether self-governance and welfare functions survived the widening.',
    'If bounding survives much wider channels, the extraction component of this tangled_rope is larger than the reading concedes and the arrangement drifts toward the snare side of the family; if bounding genuinely requires closure at this cost, more of the measured ε is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_determination_cost_justifiability, conceptual, 'Whether the exclusion cost is necessary to the coordination function or extractive excess.').

omega_variable(
    non_refoulement_hollowness_scope,
    'How far does the humanitarian carve-out reach in practice — do externalized processing, safe-third-country designations, pushbacks, and interdiction hollow the non-refoulement guarantee while preserving its form?',
    'Systematic audit of pushback documentation, recognition rates by route and procedure type, and access-to-territory data at externalized borders; litigation outcomes on collective expulsion.',
    'If the carve-out is substantially hollow, suppression and extraction are higher than authored, the rejected_asylum_seekers victim set is larger than the Refugee Convention text suggests, and the humanitarian sibling''s influence edge on this reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_refoulement_hollowness_scope, empirical, 'Whether the humanitarian exception is substantively operative or formally preserved.').

omega_variable(
    enforcement_fiscal_capture_share,
    'What share of enforcement spending delivers measurable admission control versus captured revenue and symbolic activity — and is the enforcement subsystem drifting toward theatrical self-maintenance independent of the core function?',
    'Procurement and cost-effectiveness audit: cost per removal, cost per deterred crossing, contractor revenue versus operational output; comparison of flow rates against enforcement intensity across jurisdictions.',
    'A high capture share raises the effective theater_ratio above the authored 0.40, supports piton drift of the enforcement subsystem within a live core function, and would show first in the theater series'' continued climb.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_fiscal_capture_share, empirical, 'Functional versus captured and symbolic share of the enforcement apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t10, border_legitimacy__sovereignty_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t20, border_legitimacy__sovereignty_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t30, border_legitimacy__sovereignty_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__sovereignty_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(bord_tr_t40, observed).
narrative_ontology:measurement(bord_tr_t50, border_legitimacy__sovereignty_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(bord_tr_t50, observed).
narrative_ontology:measurement(bord_tr_t60, border_legitimacy__sovereignty_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement_basis(bord_tr_t60, observed).
narrative_ontology:measurement(bord_tr_t70, border_legitimacy__sovereignty_reading, theater_ratio, 70, 0.38).
narrative_ontology:measurement_basis(bord_tr_t70, observed).
narrative_ontology:measurement(bord_tr_t80, border_legitimacy__sovereignty_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(bord_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t10, border_legitimacy__sovereignty_reading, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t20, border_legitimacy__sovereignty_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t30, border_legitimacy__sovereignty_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__sovereignty_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(bord_be_t40, observed).
narrative_ontology:measurement(bord_be_t50, border_legitimacy__sovereignty_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement_basis(bord_be_t50, observed).
narrative_ontology:measurement(bord_be_t60, border_legitimacy__sovereignty_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(bord_be_t60, observed).
narrative_ontology:measurement(bord_be_t70, border_legitimacy__sovereignty_reading, base_extractiveness, 70, 0.7).
narrative_ontology:measurement_basis(bord_be_t70, observed).
narrative_ontology:measurement(bord_be_t80, border_legitimacy__sovereignty_reading, base_extractiveness, 80, 0.71).
narrative_ontology:measurement_basis(bord_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t10, border_legitimacy__sovereignty_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t20, border_legitimacy__sovereignty_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t30, border_legitimacy__sovereignty_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__sovereignty_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement_basis(bord_su_t40, observed).
narrative_ontology:measurement(bord_su_t50, border_legitimacy__sovereignty_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(bord_su_t50, observed).
narrative_ontology:measurement(bord_su_t60, border_legitimacy__sovereignty_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(bord_su_t60, observed).
narrative_ontology:measurement(bord_su_t70, border_legitimacy__sovereignty_reading, suppression_requirement, 70, 0.76).
narrative_ontology:measurement_basis(bord_su_t70, observed).
narrative_ontology:measurement(bord_su_t80, border_legitimacy__sovereignty_reading, suppression_requirement, 80, 0.78).
narrative_ontology:measurement_basis(bord_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, asylum_non_refoulement_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'border legitimacy' covers one question — may states exclude? — that decomposes into three structurally distinct constraints, the readings of the border_legitimacy kernel. This file instantiates the sovereignty reading: exclusion presumptively legitimate, victim set = all excluded non-members, reading-indexed ε = 0.71 over the fixed referent of the standing exclusion arrangement. The freedom_of_movement sibling would author substantially higher ε over the same referent (exclusion as rights-violation rather than boundary administration); the humanitarian sibling narrows the victim set to rejected asylum seekers and authors intermediate ε. The non-refoulement regime is modeled as downstream of this reading: the humanitarian carve-out exists as a qualification whose width the sovereignty frame sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
