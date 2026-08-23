% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__guided_nationalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__guided_nationalism_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__guided_nationalism_reading
 *   human_readable: July Charter Sacred-National Legitimacy Ground (Guided-Nationalism Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-uprising transition ratified a charter whose sovereignty clauses
 *   ground legitimate authority in the polity's religious-national identity:
 *   revealed-tradition norms acquire constitutional legislative status,
 *   clerical councils acquire certification jurisdiction, and a guarantor
 *   article insulates the ratifying military. This file instantiates the
 *   guided_nationalism_reading of kernel july_charter_sovereign_legitimacy
 *   and that reading only — the secular-democratic and military-custodian
 *   readings are separate constraints (linked below), not averaged into this
 *   one. Epsilon's referent is the standing Islamic-nationalist arrangement
 *   under contest, assessed by this reading's own lights; it is neither the
 *   secular arrangement this reading's opponents endorse nor a neutral
 *   composite. The colloquial label 'the charter's sovereignty framework'
 *   decomposes into three structurally distinct claims with different
 *   epsilons, victim sets, and enforcement profiles; this file carries
 *   exactly one stable value. The claimed_type (tangled_rope) and the metrics
 *   are independent authored facts: the claim states my structural belief
 *   (genuine identity-coordination function PLUS asymmetric extraction PLUS
 *   active enforcement), while the metrics describe observed operation; where
 *   computed per-seat types diverge from the claim, that divergence is the
 *   measurement the corpus exists to take. KEY AGENTS (by structural
 *   relationship): - religious_nationalist_parties: Agenda-setter and
 *   collecting seat (organized/generational/identity_locked) — administers
 *   the framework and its proceeds - state_religious_establishment:
 *   Institutional beneficiary (institutional/generational/identity_locked) —
 *   collects jurisdiction and certification power -
 *   transitional_military_authority: Ratifying guarantor
 *   (institutional/generational/arbitrage) — hedges across competing
 *   legitimacy formulas - secular_civil_society_organizations: Primary target
 *   (moderate/biographical/constrained) — bears vetting, funding bars,
 *   prosecutions - religious_minority_communities: Primary target
 *   (powerless/biographical/constrained) — bears second-tier standing -
 *   ordinary_devout_citizens: Diffuse beneficiary
 *   (organized/biographical/identity_locked) — receives validation and
 *   distributive favor - revolutionary_youth_movements: Excluded voice
 *   (organized/immediate/constrained) — produced the uprising, sidelined in
 *   drafting - international_rights_monitors: Analytical observer
 *   (institutional/biographical/analytical) — reports and conditions aid,
 *   cannot amend the clause
 *
 * KEY AGENTS:
 *   - religious_nationalist_parties: agenda-setter and collecting seat (organized/generational/identity_locked)
 *   - state_religious_establishment: institutional beneficiary (institutional/generational/identity_locked)
 *   - transitional_military_authority: ratifying guarantor with hedged commitment (institutional/generational/arbitrage)
 *   - secular_civil_society_organizations: primary target (moderate/biographical/constrained)
 *   - religious_minority_communities: primary target (powerless/biographical/constrained)
 *   - ordinary_devout_citizens: diffuse beneficiary (organized/biographical/identity_locked)
 *   - revolutionary_youth_movements: excluded voice with residual mobilization capacity (organized/immediate/constrained)
 *   - international_rights_monitors: analytical observer, no amendment leverage (institutional/biographical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.66).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__guided_nationalism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__guided_nationalism_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__guided_nationalism_reading, "July Charter Sacred-National Legitimacy Ground (Guided-Nationalism Reading)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__guided_nationalism_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__guided_nationalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'ac6625b8-ceb2-45b0-b134-52299ef7bbcb').
narrative_ontology:cs_kernel_codification('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', fixed_text).
narrative_ontology:cs_authority_grounding('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', lineage).
narrative_ontology:cs_interpretation_layer_present('ac6625b8-ceb2-45b0-b134-52299ef7bbcb').
narrative_ontology:cs_reading_relation('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', july_charter_sovereign_legitimacy__military_custodian_reading, influences).
narrative_ontology:cs_axiom('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', foundational, religious_identity_constitutes_sovereignty).
narrative_ontology:cs_axiom_status(religious_identity_constitutes_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', religious_identity_constitutes_sovereignty, theological).
narrative_ontology:cs_axiom('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', secondary, revealed_norms_hold_constitutional_legislative_status).
narrative_ontology:cs_axiom_status(revealed_norms_hold_constitutional_legislative_status, holdable).
narrative_ontology:cs_axiom_grounding('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', revealed_norms_hold_constitutional_legislative_status, conventional).
narrative_ontology:cs_reference_frame('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', sacral_national_compact_order).
narrative_ontology:cs_drift_state('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', mid_transition_enforcement_phase, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('ac6625b8-ceb2-45b0-b134-52299ef7bbcb', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_religious_establishment).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, ordinary_devout_citizens).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society_organizations).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minority_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__guided_nationalism_reading, transitional_military_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__guided_nationalism_reading, revolutionary_youth_movements).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__guided_nationalism_reading, uprising_mandate_transmission_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Led the charter-drafting coalition after the uprising; today they staff the constitutional commission's secretariat, chair the parliamentary committee that implements conformity rulings, and appoint the officials who vet party registrations and association filings. Appointment powers, broadcast access, and discretionary funds flow through offices they hold. Abandoning the framework would dissolve the movement itself — founders' prestige, membership rolls, and donor networks all presuppose it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_parties, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_parties, beneficiary).

% Senior clerical councils received standing constitutional advisory jurisdiction: draft legislation passes before them for conformity certification, their endowments and school systems gained protected status, and their endorsements are sought at every ratification milestone. They do not run ministries or commands; they certify, opine, and accumulate jurisdiction. Their authority predates the charter by generations and would persist under any successor text, though not at the current breadth.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, state_religious_establishment, beneficiary,
    institutional, generational, identity_locked, national).

% Convened the transition after the uprising, convened and ratified the final text, and wrote itself in as guarantor: a dedicated article shields its budget from audit and assigns it defense-of-the-settlement duties. It restores order when street mobilization threatens the ratification sequence, and it hedges — lending its signature to whichever legitimacy formula keeps its prerogatives intact, which is why its ratification act is compatible with more than one reading of the same clauses.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, transitional_military_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, transitional_military_authority, beneficiary).

% Human-rights groups, independent unions, feminist networks, and bar associations ran the uprising's civic infrastructure. Under the ratified framework they face vetted registration, restrictions on foreign funding, and speech prosecutions framed as defending the religious-national consensus; several leaders carry case files. Relocation abroad is possible but hollows their domestic mandate, so they litigate, document, and protest inside steadily narrower room.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, secular_civil_society_organizations, payer,
    moderate, biographical, constrained, national).

% Confessional minorities hold recognized but second-tier standing: worship is protected, while eligibility for governorships, senior judgeships, and state-funded schooling narrows, and property and personal-status disputes route through conformity adjudication. Emigration corridors exist and remittance-dependent districts show steady outflows, but leaving means abandoning congregations, cemeteries, and family land accumulated over centuries.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_minority_communities, payer,
    powerless, biographical, constrained, national).

% The majority population finds its festivals, curriculum, family law, and public-morality codes written into the state's basic law. Public hiring, charity networks, and broadcast airtime favor conforming applicants. Their electoral weight anchors the settlement, their sense of national dignity is bound up with the text that honors their identity, and few can articulate what revising it would do for them.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, ordinary_devout_citizens, beneficiary,
    organized, biographical, identity_locked, national).

% The neighborhood committees and student fronts that toppled the old regime submitted pluralist drafts and demanded term limits and civilian oversight. Party elders and clerical councils absorbed their vocabulary while cutting their chapters in closed-door reconciliation sessions; some coordinators took advisory posts, others now face prosecutions. They retain street-mobilization capacity no other seat matches, but no institutional channel converts it into drafting power.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, revolutionary_youth_movements, excluded,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__guided_nationalism_reading, revolutionary_youth_movements, payer).

% Treaty bodies, special rapporteurs, and foreign bar associations compile periodic reviews: interviewing minority councils and prosecuted organizers, benchmarking conformity rulings against covenant obligations, and attaching findings to aid-tranche language. Their leverage is reputational and financial, running through donors and courts; they cannot amend the ground clause.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__guided_nationalism_reading, international_rights_monitors, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__guided_nationalism_reading, religious_nationalist_parties).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__guided_nationalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-collapse legitimacy-coordination problem: supplies one publicly legible answer to 'who may govern and by what title' that binds the army, parties, courts, and clerical bodies into a single ratification sequence, and standardizes family law, schooling, and public-morality administration across regions that had drifted apart under the fallen regime.
% TRANSFER_FUNCTION: Moves constitutional jurisdiction, appointment power, broadcast access, and public funds toward the religious-nationalist bloc and clerical councils; moves equal-participation standing and organizational freedom away from secular associations and minority communities; conveys the uprising's street mandate to the ratifying coalition.
% ABSENT_VOICES: Revolutionary youth coordinators attended drafting but their pluralist chapters were cut in closed-door reconciliation sessions; diaspora dissidents and prosecuted organizers were never seated; minority delegates signed under protest and their reservations sit in unpublished annexes. Seated, they would contest the ground clause's exclusivity and the guarantor article's audit shield.
% DISAPPEARANCE_RATIONALE: Overnight removal of the ground clause would reopen the succession question the charter froze: parties would re-litigate the legitimacy formula, clerical certification would lose force, the guarantor article would lose its stated object, minority-standing and registration rules would revert to interim-law patchwork, and the ratification sequence's appointments would lapse. Every seat's current position presupposes the clause stays where it is.
% FOUNDING_PROBLEM: After the uprising emptied the old regime's authority, the transition faced a triple vacuum: no agreed title to rule, an army holding physical power without a constitutional place, and a fractured polity needing one ratification path before state organs stopped functioning. The legitimacy-ground clause was written to close that vacuum quickly.
% FOUNDING_PROBLEM_CORROBORATION: Independent constitutional lawyers and the international monitors attest the vacuum was real and that ratification restored functioning government; the same outside sources dispute that closure required a confessional ground rather than civic terms. Minority councils and youth coordinators attest the vacuum persists politically wherever the clause is enforced. No attestation that the problem is settled comes from outside the ratifying coalition; the coalition's 'settled' claim stands alone, and the disagreement itself is the signal.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__guided_nationalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__guided_nationalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__guided_nationalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__guided_nationalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__guided_nationalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 because the standing arrangement moves jurisdiction, appointments, broadcast access, and equal-standing away from identifiable seats with no compensating service flow to them; the burden is decoupled from anything the payers receive. Suppression is 0.66 as a RAW structural property — registration vetting, foreign-funding bars, conformity prosecutions, and the guarantor's street-control role — deliberately unscaled; only extractiveness gets scaled by directionality and scope downstream. Theater is 0.28 and honestly moderate-low: certification, ratification sequencing, and family-law standardization are real functions, while performative piety (ceremonial conformity displays, ritualized endorsement cycles) grows slowly at the margin. Accessibility_collapse is 0.55: the pluralist alternative remains visible, drafted, and remembered — the youth movements' chapters exist — but every institutional path to it is foreclosed, and individual exit is partial (costly emigration, not blocked borders). Resistance is 0.62: protest waves, bar-association litigation, minority-council documentation, and youth boycotts are persistent and occasionally win concessions, which is exactly why the framework needs active enforcement. The measurement series run on ONE shared grid (years 0-12, every 2) so no metric borrows another's end-state; trajectories are monotone hardening, not cyclical — extraction rises as interpretive rulings activate dormant clauses, suppression rises as enforcement machinery matures, and theater creeps up as ritual accumulates. Coalition check: the powerless minority seat's realistic counterweight is a cross-confessional coalition with secular associations and youth movements; the framework's registration vetting and conformity prosecutions are aimed precisely at preventing that coalition's formation, and the outflow dynamic (see omega) drains its witness base. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit atoms captures every seat's relationship, including the military's hedge (its arbitrage-grade exit already separates it from the identity_locked establishment at the same institutional power level).
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute divergent per-seat types from this structural data, and the divergence is the finding. From the agenda-setter seats (parties, military, establishment) the arrangement is the settlement that closed a lethal vacuum: a rope-like coordination achievement they built and defend. From the payer seats (secular associations, minorities) the identical clauses operate as enforced second-class standing: snare-like. Same-power seats diverge too: the military and the clerical establishment share institutional power but differ completely in exit — the military arbitrages across legitimacy formulas, the establishment is fused to this one — so identical power atoms yield different effective positions. The youth seat is the sharpest case: organized power exceeding the secular associations', yet zero drafting access, showing that access here is gated by conformity, not capacity. The authored tangled_rope claim adjudicates none of this; the per-seat computation does.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the subsidy end: parties (d near 0.1 — they administer AND collect, the dual-positioned seat), the establishment (near 0.05 — pure jurisdiction collection), devout citizens (near 0.15 — validation and distributive favor at little direct cost). Targets cluster at the extraction end: minorities highest (powerless plus constrained exit pushes them nearest the full-target pole), secular associations slightly below (moderate power and litigation channels dampen effective extraction somewhat), youth movements pulled toward the target range through their payer secondary role despite formal exclusion. The military sits intermediate: it collects order, budget shielding, and prerogative protection (low-d pressures) while paying a minor autonomy cost under the conformity framework, and its arbitrage option pulls its effective d back toward the middle — the structural signature of a seat hedging between sibling readings. Scope is national throughout the domestic seats, which amplifies verification difficulty and hence effective extraction modestly; the observer seat's global scope carries no extraction either way.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is resolved here and none should be declared: the founding problem — closing the post-uprising legitimacy vacuum — is contested-live, attested as real by outside sources but disputed in its solution. The classification guards both mislabelings. Reading this as a pure rope erases the documented victim set and the enforcement machinery that exists precisely to hold the asymmetry; reading it as a pure snare erases the genuine coordination achievement (world_rearranges confirms the arrangement carries load no rival currently bears). It is not a scaffold: although the transition procedure presented itself as a roadmap with an electoral horizon, this reading's ground clause carries NO sunset — the procedural scaffolding expires, the legitimacy formula does not, which is exactly the tangled-rope signature of a transitional wrapper around a permanent allocation. It is not a piton: the function is alive, enforcement is strengthening, and a concentrated collecting seat (named in gain_flow) exists — theatrical maintenance is a growing symptom (theater_ratio 0.28 and rising) but the cost-asymmetry test for piton fails because the administrator profits from the current form. The R5 mismatch consumer should find status=contested paired with verdict=world_rearranges, which raises no zombie flag: the parties dispute the solution, not the problem's existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of kernel july_charter_sovereign_legitimacy (reading: guided_nationalism_reading) — which specific structural element do the sibling readings reassign, and what does each reassignment change?',
    'Compile the three sibling files and diff their structural surfaces: victim sets, beneficiary sets, authority_grounding, and enforcement metrics. The differing element is the assignment of the charter''s legitimacy-ground clause — religious-national identity (this file), armed custodianship (military_custodian_reading), or secular popular consent (secular_democratic_reading).',
    'Under the secular-democratic assignment the victim set contracts sharply and the type trends toward a transitional rope or scaffold with an electoral sunset; under the custodial assignment the beneficiary set shifts to the officer corps and enforcement metrics dominate the profile. This file''s epsilon is authored for the guided-nationalist assignment only and is not averaged across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which element of the shared charter text each sibling reading reassigns.').

omega_variable(
    constructed_vs_irreducible_legitimacy_ground,
    'Is religious identity as the ground of sovereign legitimacy an irreducible feature of this polity''s normative order that any viable charter must accommodate, or a constructed constraint whose identifiable beneficiaries are the ratifying coalition?',
    'Compare post-uprising polities that adopted civic-national legitimacy grounds without state failure; run cohort surveys of legitimacy beliefs across generations and regions; examine whether conformity demand originates in distributed popular belief or in coalition-administered institutions.',
    'If constructed, the naturality claim functions as cover for rent collection and the profile shifts toward the extractive end (false-summit-style analysis applies despite no mountain claim); if irreducible, part of the measured burden is the unavoidable price of any legitimate order and the coordination component weighs heavier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_irreducible_legitimacy_ground, conceptual, 'Whether the sacred-national ground is discovered fact or built constraint.').

omega_variable(
    suppression_structural_or_internalized,
    'Is the suppression borne by secular and minority seats structural (registration vetting, prosecution files, funding bars) or internalized (decades of majoritarian socialization producing anticipatory self-censorship that persists where enforcement is lax)?',
    'Post-amendment trajectory test: track speech, litigation, and organizing rates in jurisdictions or periods where specific enforcement tools were suspended; if suppressed activity does not rebound, a large share of suppression is internalized and travels with the agents.',
    'Internalized suppression raises the constraint''s effective suppressive force above the structural scalar and predicts persistence after any liberalizing amendment; purely structural suppression would rebound quickly once tools are withdrawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized mechanism behind measured suppression.').

omega_variable(
    identity_coordination_separability,
    'Can the identity-coordination function (one legible legitimacy formula binding army, parties, courts, and clergy) be delivered without the exclusory allocation (second-tier standing for secular associations and minorities), or are the two structurally inseparable in this polity?',
    'Natural experiment from partial reforms: if pluralist chapters (equal-participation articles, deregistered associations) are restored in isolated amendments without triggering ratification-sequence collapse, the functions are separable; if each restoration unravels the settlement, they are fused.',
    'If separable, the exclusory allocation is removable overhead riding on a genuine coordination function and the profile trends toward a cleaner rope; if fused, the measured burden substantially reflects the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_coordination_separability, conceptual, 'Whether cohesion and exclusion can be structurally unbundled.').

omega_variable(
    minority_exit_selfverification,
    'Does accelerating minority outflow make the arrangement''s suppression self-verifying by removing the witnesses, litigants, and communal infrastructure through which resistance could register?',
    'Longitudinal outflow statistics cross-tabulated against complaint filing, litigation volume, and local council activity in outflow districts.',
    'Confirmed self-verification means the authored suppression and resistance scalars understate the constraint''s trajectory — the remaining population experiences higher effective extraction than the aggregate measures suggest, and reversal becomes progressively cheaper for the coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_exit_selfverification, empirical, 'Whether emigration drains the witness base that resistance depends on.').

omega_variable(
    cs_kernel_framing_underdetermination,
    'Is the stabilizing kernel of this commitment system the charter text itself, or the coalition''s operational narrative of revolutionary success (''the uprising endured because the nation returned to its faith'') — and does the choice change the commitment-system classification?',
    'Observe what the coalition treats as unrevisable: if specific clauses are negotiable while the narrative is defended at all costs, the narrative is the operative kernel and authority_grounding shifts toward extraction (authority maintained by denying narrative revision); if clause-level revision triggers full crisis response, the fixed text is the kernel.',
    'Under the narrative-kernel framing, drift migrates into storytelling rather than interpretation, the interpretation_layer_present declaration weakens, and the coupling profile of the commitment system changes materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_kernel_framing_underdetermination, conceptual, 'Alternative framings of what the charter system''s stabilizing kernel is.').

omega_variable(
    custodial_fusion_composition,
    'Are the guided_nationalism and military_custodian readings composing into one fused operative regime — a religious legitimacy formula guaranteed by armed custodianship — rather than genuinely competing?',
    'Track whether the guarantor article is invoked to enforce conformity rulings (fusion indicator) versus invoked only against street threats to the ratification sequence (composition indicator); compare security-sector audit outcomes against the charter''s accountability chapters.',
    'Fusion raises effective suppression above this file''s authored scalar, converts this story''s network influence edge into dependence, and pushes the fused regime''s excluded-seat classifications toward the extractive end; non-fusion keeps the readings as distinct constraints with independent profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_fusion_composition, conceptual, 'Whether two sibling readings operate as one composed regime in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__guided_nationalism_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcsl_gnr_tr_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jcsl_gnr_tr_t0, observed).
narrative_ontology:measurement(jcsl_gnr_tr_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 2, 0.17).
narrative_ontology:measurement_basis(jcsl_gnr_tr_t2, observed).
narrative_ontology:measurement(jcsl_gnr_tr_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(jcsl_gnr_tr_t4, observed).
narrative_ontology:measurement(jcsl_gnr_tr_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(jcsl_gnr_tr_t6, observed).
narrative_ontology:measurement(jcsl_gnr_tr_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement_basis(jcsl_gnr_tr_t8, observed).
narrative_ontology:measurement(jcsl_gnr_tr_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(jcsl_gnr_tr_t10, observed).
narrative_ontology:measurement(jcsl_gnr_tr_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(jcsl_gnr_tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(jcsl_gnr_be_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(jcsl_gnr_be_t0, observed).
narrative_ontology:measurement(jcsl_gnr_be_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 2, 0.54).
narrative_ontology:measurement_basis(jcsl_gnr_be_t2, observed).
narrative_ontology:measurement(jcsl_gnr_be_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(jcsl_gnr_be_t4, observed).
narrative_ontology:measurement(jcsl_gnr_be_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 6, 0.61).
narrative_ontology:measurement_basis(jcsl_gnr_be_t6, observed).
narrative_ontology:measurement(jcsl_gnr_be_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 8, 0.64).
narrative_ontology:measurement_basis(jcsl_gnr_be_t8, observed).
narrative_ontology:measurement(jcsl_gnr_be_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(jcsl_gnr_be_t10, observed).
narrative_ontology:measurement(jcsl_gnr_be_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(jcsl_gnr_be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcsl_gnr_su_t0, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement_basis(jcsl_gnr_su_t0, observed).
narrative_ontology:measurement(jcsl_gnr_su_t2, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 2, 0.49).
narrative_ontology:measurement_basis(jcsl_gnr_su_t2, observed).
narrative_ontology:measurement(jcsl_gnr_su_t4, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement_basis(jcsl_gnr_su_t4, observed).
narrative_ontology:measurement(jcsl_gnr_su_t6, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 6, 0.57).
narrative_ontology:measurement_basis(jcsl_gnr_su_t6, observed).
narrative_ontology:measurement(jcsl_gnr_su_t8, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(jcsl_gnr_su_t8, observed).
narrative_ontology:measurement(jcsl_gnr_su_t10, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(jcsl_gnr_su_t10, observed).
narrative_ontology:measurement(jcsl_gnr_su_t12, july_charter_sovereign_legitimacy__guided_nationalism_reading, suppression_requirement, 12, 0.66).
narrative_ontology:measurement_basis(jcsl_gnr_su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__guided_nationalism_reading, identity_coordination).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__guided_nationalism_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial object 'the July Charter's sovereignty settlement' covers three structurally distinct claims with different stable epsilons and victim sets. This file is the guided_nationalism_reading (religious identity as sovereign ground; victims include secular civil society and religious minorities). The military_custodian_reading is upstream in ratification fact — the guarantor article is cited by all three readings — and this reading creates downstream pressure on it by changing what the guardianship defends. The secular_democratic_reading is the direct rival assignment of the ground clause and is foreclosed within the enacted framework while remaining a live held position in the wider polity. All three files link one another via network.affects_constraints; contamination propagation should treat the ground-clause clause-set as the shared surface across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
