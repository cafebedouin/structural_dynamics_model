% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Foundational State Authority to Exclude Non-Members (Sovereignty-Primary Reading of Border Normative Status)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primary reading of the contested
 *   border_normative_status kernel: territorial boundaries as legitimate
 *   instruments of collective self-determination, with states holding
 *   foundational authority to exclude non-members. The referent of every
 *   metric is the standing arrangement under contest — the actual global
 *   border regime of patrolled boundaries, visa hierarchies, detention, and
 *   deportation — assessed by this reading's own lights, never by the
 *   freedom_primary sibling's. On this reading the arrangement is claimed as
 *   legitimate coordination: a demos must be bounded for self-rule and
 *   solidarity to have a subject. The metrics are authored independently of
 *   that claim and record what the arrangement actually does to its parties:
 *   it delivers bounded-membership goods to citizens and enforcement mandates
 *   to agencies, while imposing route mortality, detention, and rightlessness
 *   on migrants who hold no vote anywhere in the system that stops them. The
 *   expected structural delta is honored: excluded migrants sit in the victim
 *   set, enforcement reads as a legitimate state function, and burdens on
 *   non-members register as prices of membership rather than injuries
 *   demanding redress. Sibling readings are separate files linked through the
 *   network block; nothing about them is averaged into this one. KEY AGENTS
 *   (by structural relationship): - citizen_demos: Primary beneficiary
 *   (organized/constrained) — collects bounded-membership goods: welfare
 *   solidarity, labor protection, self-rule - border_enforcement_apparatus:
 *   Agenda-setter (institutional/constrained) — administers and enforces
 *   exclusion; converts the principle into budgets, patrols, detention beds -
 *   excluded_migrants: Primary target (powerless/trapped) — bears denial of
 *   entry, route risk, death - asylum_seekers: Primary target
 *   (powerless/trapped) — bears interception before claim, detention,
 *   pushback - undocumented_residents: Dual-positioned target-beneficiary
 *   (powerless/identity_locked) — lives inside the line it cannot cross
 *   legally - employers_of_rightless_labor: Secondary beneficiary
 *   (powerful/arbitrage) — captures the labor surplus that rightlessness
 *   produces - human_rights_review_bodies: Analytical observer
 *   (institutional/analytical) — audits the regime against obligations the
 *   reading accepts at its margins
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.48).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.68).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.48).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Foundational State Authority to Exclude Non-Members (Sovereignty-Primary Reading of Border Normative Status)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'c6b5135e-8804-4198-96ba-9a7b712e8986').
narrative_ontology:cs_kernel_codification('c6b5135e-8804-4198-96ba-9a7b712e8986', formalized).
narrative_ontology:cs_authority_grounding('c6b5135e-8804-4198-96ba-9a7b712e8986', lineage).
narrative_ontology:cs_interpretation_layer_present('c6b5135e-8804-4198-96ba-9a7b712e8986').
narrative_ontology:cs_reading_relation('c6b5135e-8804-4198-96ba-9a7b712e8986', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('c6b5135e-8804-4198-96ba-9a7b712e8986', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('c6b5135e-8804-4198-96ba-9a7b712e8986', foundational, foundational_collective_exclusion_authority).
narrative_ontology:cs_axiom_status(foundational_collective_exclusion_authority, holdable).
narrative_ontology:cs_axiom_grounding('c6b5135e-8804-4198-96ba-9a7b712e8986', foundational_collective_exclusion_authority, conventional).
narrative_ontology:cs_axiom('c6b5135e-8804-4198-96ba-9a7b712e8986', foundational, self_determination_requires_bounded_membership).
narrative_ontology:cs_axiom_status(self_determination_requires_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('c6b5135e-8804-4198-96ba-9a7b712e8986', self_determination_requires_bounded_membership, instrumental).
narrative_ontology:cs_reference_frame('c6b5135e-8804-4198-96ba-9a7b712e8986', westphalian_exclusive_territoriality).
narrative_ontology:cs_drift_state('c6b5135e-8804-4198-96ba-9a7b712e8986', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c6b5135e-8804-4198-96ba-9a7b712e8986', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_demos).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, border_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, employers_of_rightless_labor).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, undocumented_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, undocumented_residents).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, westphalian_territorial_integrity_principle).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, bounded_demos_legitimacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of bounded political communities. They receive the goods the boundary secures: a welfare pool with definite contributors, labor-market protection from unlimited competition, and a political process they govern through their own votes. Their exit — emigration and naturalization elsewhere — exists but costs language, career, and family ties, so most stay and sustain the arrangement they benefit from.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_demos, beneficiary,
    organized, generational, constrained, national).

% Border guards, immigration ministries, asylum adjudication systems, detention and deportation services. They translate the exclusion principle into daily operations: patrols, visa refusals, removal flights, case backlogs. The arrangement is their mandate and their budget line; institutional growth scales with enforcement intensity. They answer to legislatures elected by the membership, not to anyone they stop.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, border_enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% People who want or need to enter and are refused: denied visas, intercepted at frontiers, or pushed back along routes. They bear the sharpest costs — route mortality in deserts and seas, years in transit limbo, permanent separation from relatives who hold the right papers. Their alternative is remaining where they are, which for many is the condition they are fleeing. No polity they can reach gives them a vote on the rules that stop them.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% People at borders claiming protection from persecution. The regime processes them through detention, dispersal, and multi-year adjudication, and increasingly intercepts them before they can lodge a claim at all — externalized to transit states or turned back at sea. Their legal position depends on proving their danger after arrival, while enforcement works to ensure arrival never happens.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, biographical, trapped, global).

% People living and working inside the territory without authorization — visa overstayers, refused applicants who never left, entrants who evaded patrol. They hold jobs, raise children in local schools, and build decades of life in the place, all under standing liability to arrest and removal. Leaving voluntarily means abandoning homes, partners, and citizen children; staying means a life without status. Many pay taxes and send remittances home while unable to claim the benefits they fund.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, undocumented_residents, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, undocumented_residents, beneficiary).

% Agricultural, construction, care, and hospitality employers who hire from the unauthorized workforce. Lack of status keeps workers available, docile, and cheap: they cannot approach labor inspectors without risking removal. These employers gain a labor surplus the boundary itself manufactures, and they can relocate production or recruitment across regions if enforcement tightens.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, employers_of_rightless_labor, beneficiary,
    powerful, biographical, arbitrage, continental).

% Treaty bodies, regional courts, and special rapporteurs that audit the regime against obligations the sovereignty tradition itself accepts at its margins — non-refoulement, family unity, detention safeguards. They issue judgments and reports that states implement slowly, partially, or openly defy; they can shame and occasionally enjoin, but command no patrols.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_review_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, citizen_demos).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines a determinate membership for a political community so that collective decisions, mutual obligation, and redistributive solidarity have a bounded subject; coordinates who may occupy territory, work, vote, and draw on public services.
% TRANSFER_FUNCTION: Moves access to territory, labor markets, housing, and public services from non-members to members; moves the risks of movement — route mortality, detention, family separation — onto those seeking entry; moves enforcement costs onto public budgets and remittance income out of destination economies.
% ABSENT_VOICES: Excluded migrants and asylum seekers hold no vote in the polities whose rules exclude them; families separated by refusal decisions, origin communities losing members to route deaths, and the deterred who never attempt travel have no seat anywhere in the process. Their objections reach the conversation only indirectly, through NGOs, treaty-body reports, and litigation brought by citizens on their behalf.
% DISAPPEARANCE_RATIONALE: If foundational exclusion authority vanished overnight, every welfare state would face an unanswered question about who its services are for, labor markets would reprice around unrestricted entry, citizenship's premium would collapse, and the architecture of democratic accountability — which presupposes a fixed 'we' — would need reconstruction within months.
% FOUNDING_PROBLEM: Constituting a determinate political community capable of collective self-government and mutual obligation: after Westphalia, deciding who belongs to the people that rules itself, so that majority decisions bind someone in particular and redistribution has a definite circle of contributors and recipients.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: cosmopolitan theorists who reject the sovereignty reading (Carens, Benhabib) nonetheless attest that bounded-community problems are real and demand answers; ICCPR common Article 1 entrenches self-determination in positive international law; UNHCR field reporting and origin-state governments independently document membership governance as a live operational problem, not a beneficiary invention.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.48 is authored from this reading's own lights over the standing arrangement: the core exclusion function is legitimate self-determination on this reading, but the arrangement's actual operation includes costs the reading's own marginal commitments cannot absorb — route mortality absorbed as acceptable risk, externalized pushback toward danger, a rightless labor stratum, family separation. Hence mid-range: far below what the freedom_primary sibling would author over the same referent, and below the qualified reading's likely value. Suppression 0.68 is raw and unscaled: patrol force, detention, and deportation are physical facts of the arrangement, not context-dependent quantities. Theater 0.30: wall-building and deployment spectacle perform control while the operative machinery is visa regimes and interior enforcement. Accessibility_collapse 0.50: alternatives demonstrably function at scale (free movement among EU member states), so alternatives do not collapse on inspection. Resistance 0.60: sanctuary networks, treaty litigation, NGO documentation, and smuggling counter-institutions meet the regime continuously. Temporally, the interval spans the postwar guest-worker era (porous labor recruitment, modest enforcement) through restriction waves, asylum hardening, post-2001 securitization, and externalization agreements: enforcement capacity ratchets steadily upward, extractiveness creeps up as enforcement intensifies faster than the legitimate core expands, and theater rises as visible spectacle substitutes for and advertises control. All three series share one grid (t=0..72, seven points) so no metric borrows another's endpoint.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From the citizen_demos seat the arrangement presents as ordinary coordination: taxes paid, services bounded, self-rule meaningful. From the excluded_migrant seat the same wall is pure barrier: coercion, no voice, no exit. The enforcement apparatus experiences neither — it experiences mandate, budget, and caseload. Nothing in the structure changes across seats; only position does. The engine derives this divergence from the directionalities; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   citizen_demos declares beneficiary: subsidized by the boundary, d near the beneficiary end. employers_of_rightless_labor declares beneficiary with arbitrage-grade exit: it captures the labor surplus rightlessness produces and can relocate if enforcement tightens, so d sits low. border_enforcement_apparatus is agenda_setter: it collects appropriations and mandate rather than the primary rents, placing it near-symmetric with a slight beneficiary tilt. excluded_migrants and asylum_seekers declare victim with trapped exit: d near full-target, amplified by global scope and the difficulty of verifying treatment along remote routes. undocumented_residents declare victim with identity_locked exit: the lock pushes them toward the target end despite the incidental residence benefits their secondary role records. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already place every seat correctly, and a single power-atom override would have distorted the pure-target seats alongside the mixed one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constituting a bounded demos — remains live, so the arrangement is not running on a dead mandate and mandatrophy_resolved stays undeclared. The classification discipline cuts both ways here: reading the arrangement as pure extraction would erase the genuine self-determination function that even the reading's critics concede, while accepting the reading's own legitimacy claim wholesale would erase the voiceless cost-bearers the reading's own marginal commitments (non-refoulement, family unity) acknowledge. The tangled_rope claim holds both halves apart: coordination with a real subject, extraction with real victims, held together by active enforcement. Status=live crossed with verdict=world_rearranges yields no zombie flag: the mandate and the dependence are both real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates only the sovereignty_primary reading of the border_normative_status kernel; what structural changes would instantiating freedom_primary or qualified_sovereignty instead produce?',
    'Comparative classification across the three sibling stories: diff the victim sets, enforcement-legitimacy flags, and epsilon values authored over the same referent arrangement.',
    'freedom_primary would move excluded migrants from cost-bearing outsiders to rights-holders whose exclusion is itself the violation (epsilon rises sharply and the enforcement apparatus flips from legitimate administrator to primary aggressor); qualified_sovereignty would shrink the victim set to disproportionately excluded classes and re-scope enforcement as conditionally legitimate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame indexicality: one of three readings of the border-normative-status kernel.').

omega_variable(
    demos_boundary_naturalness,
    'Is bounded-membership authority a contingent construction maintained by enforcement, or a functional requirement of large-scale democratic self-government that would re-emerge under any alternative?',
    'Compare polities that dissolved internal boundaries (EU free movement) and historical open-border episodes: does democratic self-rule and redistributive solidarity survive membership openness, and at what scale?',
    'If functional necessity, part of the measured cost sits near the coordination floor and the arrangement resists pure-extraction readings; if contingent, the full weight falls on enforcement choices and the extractive share grows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demos_boundary_naturalness, empirical, 'Whether the demos boundary is constructed or functionally required.').

omega_variable(
    principle_vs_practice_extraction,
    'How much of the measured cost load belongs to the sovereignty principle itself versus particular enforcement practices (externalized pushback, detention conditions, route militarization) that even sovereignty-tradition theorists condemn?',
    'Decompose observed harms by practice type and test each against the reading''s own accepted limits (non-refoulement, proportionality at the margins); practices surviving the reading''s internal critique attribute to the principle.',
    'A high practice-attributable share keeps epsilon near the coordination floor; a high principle-attributable share pushes the arrangement toward pure extraction even on this reading''s own lights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_vs_practice_extraction, empirical, 'Attribution of harm between the principle and its enforcement practices.').

omega_variable(
    undocumented_exit_lock_mechanism,
    'Is the undocumented resident''s inability to leave structural (legal bars, asset immobility, citizen children''s schooling) or internalized (rootedness, community fusion, fear instilled by enforcement)?',
    'Post-regularization trajectories: if departure rates and expressed attachment shift sharply when legal status changes, the lock was structural; if attachment persists independent of status, it is internalized.',
    'An internalized lock raises the effective hold on this seat above what legal analysis alone suggests; a structural lock routes remedies toward legalization pathways rather than mobility support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(undocumented_exit_lock_mechanism, empirical, 'Structural vs internalized mechanism behind the undocumented resident''s immobility.').

omega_variable(
    deterred_non_attemptee_victimhood,
    'Does the victim set include potential migrants deterred before attempting crossing — people who never enter the enforcement encounter at all?',
    'Estimate the deterred population via origin-country survey data on suppressed migration intentions and compare realized flows against pre-restriction baselines.',
    'Including the deterred enlarges the victim set severalfold and raises the arrangement''s total cost load; excluding it confines victims to those physically intercepted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterred_non_attemptee_victimhood, conceptual, 'Boundary of the victim set: intercepted only, or also deterred.').

omega_variable(
    migrant_coalition_capacity,
    'Can the powerless seats convert diffuse numbers into coalition power — sanctuary networks, transnational advocacy, labor organizing — sufficient to alter enforcement?',
    'Track outcomes of sanctuary jurisdictions, migrant-led campaigns, and cross-border union drives against enforcement intensity over time.',
    'Rising coalition capacity lowers the effective suppression the arrangement can sustain and shifts per-seat classifications for the powerless seats; persistent failure confirms isolated-target dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migrant_coalition_capacity, empirical, 'Coalition-power potential of the excluded seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bord_tr_t12, border_normative_status__sovereignty_primary, theater_ratio, 12, 0.2).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__sovereignty_primary, theater_ratio, 24, 0.23).
narrative_ontology:measurement(bord_tr_t36, border_normative_status__sovereignty_primary, theater_ratio, 36, 0.26).
narrative_ontology:measurement(bord_tr_t48, border_normative_status__sovereignty_primary, theater_ratio, 48, 0.28).
narrative_ontology:measurement(bord_tr_t60, border_normative_status__sovereignty_primary, theater_ratio, 60, 0.29).
narrative_ontology:measurement(bord_tr_t72, border_normative_status__sovereignty_primary, theater_ratio, 72, 0.3).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(bord_be_t12, border_normative_status__sovereignty_primary, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(bord_be_t24, border_normative_status__sovereignty_primary, base_extractiveness, 24, 0.43).
narrative_ontology:measurement(bord_be_t36, border_normative_status__sovereignty_primary, base_extractiveness, 36, 0.45).
narrative_ontology:measurement(bord_be_t48, border_normative_status__sovereignty_primary, base_extractiveness, 48, 0.46).
narrative_ontology:measurement(bord_be_t60, border_normative_status__sovereignty_primary, base_extractiveness, 60, 0.47).
narrative_ontology:measurement(bord_be_t72, border_normative_status__sovereignty_primary, base_extractiveness, 72, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(bord_su_t12, border_normative_status__sovereignty_primary, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(bord_su_t24, border_normative_status__sovereignty_primary, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(bord_su_t36, border_normative_status__sovereignty_primary, suppression_requirement, 36, 0.6).
narrative_ontology:measurement(bord_su_t48, border_normative_status__sovereignty_primary, suppression_requirement, 48, 0.64).
narrative_ontology:measurement(bord_su_t60, border_normative_status__sovereignty_primary, suppression_requirement, 60, 0.66).
narrative_ontology:measurement(bord_su_t72, border_normative_status__sovereignty_primary, suppression_requirement, 72, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the ethics of borders' covers three structurally distinct claims with different epsilons, victim sets, and enforcement logics. This file is the sovereignty_primary member; it links to its two siblings. The upstream/downstream structure runs from this baseline reading outward: the qualified reading is articulated as a modification of the sovereignty baseline, and the freedom reading defines itself against it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
