% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Qualified Sovereignty: Proportionality-Limited Border Control
 *   domain: political philosophy/international law/migration
 *
 * SUMMARY:
 *   The standing arrangement under contest: states operate border-control
 *   regimes — visa regimes, interception, detention, removal, externalized
 *   enforcement — that decide who enters, stays, and is expelled. This story
 *   instantiates the qualified_sovereignty reading of the
 *   border_normative_status kernel: border control is legitimate in principle
 *   as an instrument of collective self-determination and public order, but
 *   its exercise is reviewable and must be justified, necessary,
 *   proportionate, and consistent with human rights obligations including
 *   non-refoulement. Per the kernel-reading ε rule, ε's referent is that
 *   standing arrangement as this reading sees it — never the fully compliant
 *   regime the reading would endorse. By this reading's lights the
 *   arrangement is substantially but not purely extractive: a genuine
 *   coordination core wrapped around systematic proportionality failures
 *   (pushbacks at land and sea borders, externalization that moves
 *   refoulement beyond jurisdiction, detention without individualized
 *   review). The sibling readings are separate constraints, not hedges inside
 *   this one: sovereignty_primary reads the same arrangement as near-pure
 *   coordination; freedom_primary reads it as near-pure rights violation.
 *   Assumptions stated: the interval maps 1951–2026 (Refugee Convention to
 *   present) with points at regime milestones; 'displaced citizens' is read
 *   as citizens displaced abroad whose own state cannot protect them; the
 *   receipt seat is the apparatus because enforcement budgets, databases, and
 *   agency growth demonstrably accrue there. The claim and the metrics are
 *   independent authored facts: claimed_type is the reading's structural
 *   assessment, the metrics describe observed operation, and the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - state_border_apparatus: agenda-setter (institutional/constrained) — administers the regime, collects its institutional growth, bears the adjudication burden the reading imposes
 *   - international_human_rights_bodies: co-agenda-setter (institutional/analytical) — enforce the proportionality qualification through treaty review and litigation
 *   - resident_citizens: primary beneficiary (organized/mobile) — collect the membership goods the boundary preserves; bear enforcement taxes and family/moral costs secondarily
 *   - excluded_migrants: primary target (powerless/trapped) — bear refusal, detention, removal, and the deadly-route dilemma
 *   - displaced_citizens: secondary target (powerless/trapped) — displaced by conflict and disaster, refused the refuge that proportionate exercise would have weighed
 *   - transit_and_host_states: dual-positioned intermediary (moderate/constrained) — paid hosting burdens, compensated with aid and gatekeeper leverage
 *   - refugee_advocacy_organizations: analytical observer (organized/mobile) — documents, litigates, publishes the compliance record
 *   - transit_zone_interceptees: excluded seat (powerless/trapped) — decided upon before any forum can reach them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.66).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.62).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.66).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Qualified Sovereignty: Proportionality-Limited Border Control").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political philosophy/international law/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, '23583051-d592-4552-af16-81bce899915f').
narrative_ontology:cs_kernel_codification('23583051-d592-4552-af16-81bce899915f', formalized).
narrative_ontology:cs_authority_grounding('23583051-d592-4552-af16-81bce899915f', lineage).
narrative_ontology:cs_interpretation_layer_present('23583051-d592-4552-af16-81bce899915f').
narrative_ontology:cs_reading_relation('23583051-d592-4552-af16-81bce899915f', border_normative_status__sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('23583051-d592-4552-af16-81bce899915f', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('23583051-d592-4552-af16-81bce899915f', foundational, border_authority_conditionally_legitimate).
narrative_ontology:cs_axiom_status(border_authority_conditionally_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('23583051-d592-4552-af16-81bce899915f', border_authority_conditionally_legitimate, deontological).
narrative_ontology:cs_axiom('23583051-d592-4552-af16-81bce899915f', secondary, proportionality_review_mandatory).
narrative_ontology:cs_axiom_status(proportionality_review_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('23583051-d592-4552-af16-81bce899915f', proportionality_review_mandatory, conventional).
narrative_ontology:cs_reference_frame('23583051-d592-4552-af16-81bce899915f', proportionate_rights_limited_border_authority).
narrative_ontology:cs_drift_state('23583051-d592-4552-af16-81bce899915f', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('23583051-d592-4552-af16-81bce899915f', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, state_border_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, resident_citizens).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, transit_and_host_states).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, resident_citizens).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, transit_and_host_states).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, proportionality_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__qualified_sovereignty, non_refoulement_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets visa policy, runs interception and detention, executes removals, and negotiates externalization agreements with transit states. Collects the discretion, budgets, biometric databases, and agency growth the regime generates, and pays the adjudication and compliance costs that treaty review imposes. Exit would mean denouncing the treaties that bind it — possible in form, costly in standing and reciprocity.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, state_border_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Live inside the boundary the regime maintains: they receive the welfare, security, and self-determination goods that membership preserves, and vote for the governments that set it. They also fund enforcement through taxation, absorb labor-market and family-separation effects, and carry the moral cost of refusals at the border. They can leave, though emigration is costly and one-way.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, resident_citizens, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, resident_citizens, payer).

% Seek entry or stay and are refused, detained, or removed under rules they had no part in making; many route through deadly crossings because legal channels are narrow or absent. Returning home may be unsafe; staying put may be untenable. Their options are the routes the regime leaves open, and those are the most dangerous ones.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Fled war, persecution, or disaster in their own state, which cannot or will not protect them. Whether they find refuge depends on other states' admission decisions; when those decisions skip any justification-and-necessity inquiry they are refused outright or pushed back. Their own passport offers them no protection at the borders they reach.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens, payer,
    powerless, biographical, trapped, global).

% Sit along the routes and host the displaced. They receive aid packages, trade concessions, and diplomatic leverage in exchange for acting as destination states' gatekeepers, and they bear the hosting burdens, camp economies, and refoulement-by-proxy exposure the role carries. Their bargaining position depends on continued flows, which gives them a stake in the regime's persistence.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, transit_and_host_states, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, transit_and_host_states, beneficiary).

% Hear individual complaints, issue interim measures, and publish judgments testing border measures against justification, necessity, and proportionality standards. Their docket and doctrinal authority grow with each case; their remedies depend on state compliance they cannot directly compel.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, international_human_rights_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Document conditions at borders, litigate test cases, and publish states' compliance records. They hold no formal decision power; their leverage is evidence, litigation standing, and public opinion.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, refugee_advocacy_organizations, observer,
    organized, biographical, mobile, global).

% Are picked up at sea or held in transit zones before reaching any territory where a complaint can be filed. No court, advocate, or recording officer is present at the moment decisions about them are made; they enter the record, if at all, only after the fact.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, transit_zone_interceptees, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__qualified_sovereignty, state_border_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__qualified_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates admission to state territories: maintains the membership boundary, sequences and filters flows, and allocates access to scarce public goods (welfare, housing, services) within a bounded political community; provides states a shared framework for predictable migration management.
% TRANSFER_FUNCTION: Moves decision authority over human mobility from individuals to state discretion bounded by rights review; moves the costs of exclusion (denied entry, detention, removal, route danger) onto migrants and the displaced, and moves adjudication and enforcement costs onto states and taxpayers; moves aid and leverage to transit states in exchange for gatekeeping.
% ABSENT_VOICES: Intercepted migrants at sea and in transit zones are structurally absent — decisions about them are made before any forum can hear them. Transit-state populations bearing hosting burdens are consulted through aid frameworks but not as principals. Future migrants affected by today's precedent-setting exclusions have no seat anywhere in the process.
% DISAPPEARANCE_RATIONALE: If the proportionality qualification and its adjudicatory machinery vanished overnight, border control would continue (the sovereignty component is independent) but unchecked: refoulement without review, detention without individualized assessment, externalization without a compliance record. The treaty-body docket, the pushback litigation line, and the compliance-reporting architecture would dissolve, and the arrangement's extraction would no longer be documented or contestable — the world of border practice would rearrange around unreviewable discretion.
% FOUNDING_PROBLEM: After 1945, two catastrophes had to be reconciled: the ethnic-exclusion regimes that had made borders instruments of persecution, and the refugees whom no state would admit. The arrangement was built to keep territorial authority while making its exercise reviewable — borders remain, but exclusion must be justified, necessary, proportionate, and consistent with human rights obligations, with non-refoulement as the floor.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the drafting histories of the 1951 Refugee Convention and the European Convention on Human Rights record state parties conceding the pre-war failures; ECtHR jurisprudence (Hirsi Jamaa v. Italy; N.D. and N.T. v. Spain; Ilias v. Hungary) and UNHCR mandate documentation attest the problem's persistence independently of state self-assessment; migration-law scholarship unaffiliated with state interests corroborates both the problem's liveness and the gap between standard and practice. States themselves attest it only when justifying restrictions, which is self-interested and treated accordingly.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.66: by this reading's lights the regime retains a legitimate coordination core (orderly admission, membership boundary) while systematically failing the justification-necessity-proportionality test it is supposed to pass. Suppression (0.62) is the raw structural coercion the arrangement requires — carrier sanctions, interception, biometric databases, detention capacity — authored unscaled per the framework rule; only extractiveness is scaled downstream by directionality and scope. Theater (0.42) reflects adjudication that is formally elaborate and substantively thin: near-zero interim-measure grant rates in some corridors, judgments unimplemented for years, procedural expansion that outpaces remedy. Accessibility collapse (0.50): legal channels exist but are narrow; irregular alternatives persist at lethal cost, so alternatives are degraded rather than eliminated — the tangled-rope profile, not the snare's near-total closure. Resistance (0.55): sustained Strasbourg and national litigation, NGO documentation, interstate friction, and migrant persistence despite route mortality. The three series share one grid (1951, 1975, 1993, 2001, 2015, 2026). Extraction and theater rise together — formal rights machinery grows while practice externalizes, which is this reading's central complaint — and suppression_requirement dips slightly after 2015 as courts constrain the worst practices. Coordination type is authored as resource_allocation: the function whose failure the regime exists to prevent is unmanaged admission to territory and the public goods inside it; membership-boundary maintenance serves that allocation, so the identity framing is instrumental here, not primary.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the state apparatus seat the arrangement looks like a burdened instrument: it built the regime and now answers to courts for exercising it — the adjudication burden this reading explicitly names. From the migrant and displaced seats the same arrangement is the thing done to them: refusal, detention, removal, and the choice between a dangerous route and an untenable home. Resident citizens experience a benefit with diffuse costs; transit states experience paid intermediation whose rents depend on the flows continuing; the human rights bodies experience growing jurisdiction and docket. One structure, divergent computed types per seat — the engine derives this divergence from power, exit, and role, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the apparatus collects discretion, budgets, and institutional growth (d near the beneficiary end); citizens collect the membership goods, damped toward symmetric by their payer side-costs. Victim declarations map the other way: excluded migrants and displaced citizens bear refusal, detention, and route danger with trapped exit — no legal channel home or onward — which pushes them to the full-target end and maximizes their effective extraction; their global scope further amplifies it through the scope modifier. Transit states are the one seat the derivation would misread: their primary payer role would derive a high d, but they simultaneously collect aid, trade concessions, and gatekeeper rents, so an override sets the moderate power atom to d = 0.55. Human rights bodies derive near the beneficiary end without override — jurisdiction and doctrinal authority accrue to them with each case. Advocacy organizations are analytical and feed no directionality. Suppression stays unscaled; scope amplification applies to extraction only, which is why the trapped, global migrant seats carry the story's maximum computed χ.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling territorial authority with the rights of people at borders after 1945 — is live and arguably intensifying with record global displacement, so no mandatrophy is declared and the arrangement is no vestige. The classification work is preventive: the sovereignty_primary reading would label the arrangement pure coordination and erase the migrant seats; the freedom_primary reading would label it pure extraction and erase both the coordination function and the burden the arrangement imposes on states. The tangled_rope structure keeps both facts in view and names the drift signal: theater_ratio has climbed from 0.15 to 0.42 while extraction rose in step. If review continues decoupling from practice — ritual adjudication over a regime whose fixing cost is prohibitive and whose gains accrue to the apparatus — the arrangement drifts toward the piton cell, with proportionality review as performance. The rising theater series is the early warning; the adjudication-genuineness omega is the instrument that would confirm it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_epsilon,
    'This story instantiates the qualified_sovereignty reading of the border_normative_status kernel and authors ε by its lights; what ε would the sibling readings author over the same standing arrangement, and where exactly is the disagreement located?',
    'Generate the sibling stories (border_normative_status__sovereignty_primary, border_normative_status__freedom_primary) with identical stakeholder surface and interval; compare authored ε and computed per-seat types across the family.',
    'sovereignty_primary would author ε near the coordination end (exclusion as legitimate self-determination, costs as coordination overhead); freedom_primary would author ε near the maximum (exclusion as rights violation). The disagreement is located in the baseline legitimacy of exclusion, not in the facts of enforcement, which all three readings share. This story''s tangled_rope claim is reading-indexed, not topic-absolute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_epsilon, conceptual, 'ε is a property of the reading; sibling readings of one kernel author different ε over the same referent.').

omega_variable(
    adjudication_theater_ambiguity,
    'Is proportionality review a functioning limit on state border exercise, or a legitimating ritual whose grant rates and compliance rates are near zero?',
    'Track judgment-compliance rates, interim-measure grant rates (e.g., ECtHR Rule 39 indications), and exclusion-rate deltas before and after review in comparable migration corridors.',
    'If review is largely theatrical, theater_ratio is understated at 0.42, the arrangement drifts toward the piton profile (ritual maintenance, prohibitive fixing cost), and ε rises because review legitimizes rather than limits. If review genuinely redirects state practice, the coordination side is stronger than authored and the arrangement sits closer to a workable hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adjudication_theater_ambiguity, empirical, 'Whether the proportionality layer constrains practice or performs constraint.').

omega_variable(
    externalization_attribution_boundary,
    'Do externalized enforcement arrangements — migration deals with transit states, offshore processing, pushback-by-proxy — belong to this constraint''s operation, or to a distinct constraint centered on transit-state gatekeeping?',
    'Decompose per the ε-invariance rule: author a separate story for the externalization architecture with its own ε and victim set, link via network.affects_constraints, and test whether each story''s ε is stable under its own framing.',
    'If externalization is inside this constraint, ε here is understated (extraction displaced beyond the jurisdiction where review operates); if outside, this story''s ε measures only in-territory exercise and the family carries the displaced extraction in the sibling story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalization_attribution_boundary, conceptual, 'Boundary of the constraint''s operation where enforcement is moved beyond reviewable territory.').

omega_variable(
    displaced_citizen_attribution,
    'Are displaced citizens genuine victims of the border-control arrangement, or victims of the conflicts and disasters that displaced them, with the border regime merely failing to remedy?',
    'Counterfactual corridor analysis: in documented crises, would proportionate, rights-consistent border exercise have admitted the displaced? Compare corridors where the justification-necessity-proportionality inquiry was applied against corridors where it was skipped.',
    'If they are victims of remedy-failure rather than of the arrangement, the victim set contracts to excluded_migrants, citizen-seat directionality shifts toward the beneficiary end, and effective extraction concentrates further on the migrant seats — raising the asymmetry this reading condemns.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_citizen_attribution, conceptual, 'Attribution of the displaced-citizen victim class to the arrangement versus to the displacing events.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 1951, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1951, border_normative_status__qualified_sovereignty, theater_ratio, 1951, 0.15).
narrative_ontology:measurement(bord_tr_t1975, border_normative_status__qualified_sovereignty, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(bord_tr_t1993, border_normative_status__qualified_sovereignty, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(bord_tr_t2001, border_normative_status__qualified_sovereignty, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__qualified_sovereignty, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(bord_tr_t2026, border_normative_status__qualified_sovereignty, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t1951, border_normative_status__qualified_sovereignty, base_extractiveness, 1951, 0.44).
narrative_ontology:measurement(bord_be_t1975, border_normative_status__qualified_sovereignty, base_extractiveness, 1975, 0.47).
narrative_ontology:measurement(bord_be_t1993, border_normative_status__qualified_sovereignty, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(bord_be_t2001, border_normative_status__qualified_sovereignty, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__qualified_sovereignty, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(bord_be_t2026, border_normative_status__qualified_sovereignty, base_extractiveness, 2026, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1951, border_normative_status__qualified_sovereignty, suppression_requirement, 1951, 0.32).
narrative_ontology:measurement(bord_su_t1975, border_normative_status__qualified_sovereignty, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(bord_su_t1993, border_normative_status__qualified_sovereignty, suppression_requirement, 1993, 0.48).
narrative_ontology:measurement(bord_su_t2001, border_normative_status__qualified_sovereignty, suppression_requirement, 2001, 0.56).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__qualified_sovereignty, suppression_requirement, 2015, 0.64).
narrative_ontology:measurement(bord_su_t2026, border_normative_status__qualified_sovereignty, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% The colloquial label 'border control' decomposes, per the ε-invariance principle, into three structurally distinct constraints — one per reading of the border_normative_status kernel. They share a referent (standing border-control practice) but differ in ε: sovereignty_primary authors near-zero extraction (exclusion as collective self-determination), freedom_primary near-maximal (exclusion as rights violation), qualified_sovereignty intermediate-high (legitimate core, systematic proportionality failure). The upstream reading (sovereignty_primary) supplies the legitimacy premise this reading qualifies; this reading supplies the adjudicatory machinery that freedom_primary's litigation runs on. All three are linked via affects_constraints so contamination and drift propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__qualified_sovereignty, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
