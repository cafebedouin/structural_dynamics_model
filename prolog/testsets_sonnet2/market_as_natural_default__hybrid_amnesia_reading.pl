% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default (Two-Stage Amnesia/Capture Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This story instantiates the hybrid_amnesia_reading of the
 *   market_as_natural_default kernel: a two-stage account in which the
 *   1930s-1970s narrowing of taught and institutionalized allocation
 *   frameworks was a largely genuine, low-intentionality forgetting (driven
 *   by Cold War academic realignment and wartime technocratic centralization,
 *   not by deliberate beneficiary engineering), while the 1980s-present
 *   period shows a distinct and traceable shift into defensive
 *   rationalization by parties who inherited — but did not originally create
 *   — the resulting gap. The extractiveness trajectory (0.20 to 0.45) is
 *   authored to reflect this: low and roughly flat through the amnesia phase,
 *   then rising as incumbent intermediaries, asset owners, and the
 *   professoriate convert an inherited default into an actively defended
 *   position. This is a different constraint from lapsed_alternative_reading
 *   (which claims the whole period is innocent forgetting with no active
 *   capture) and from beneficiary_maintained_reading (which claims active
 *   defense runs the entire timeline, including the earlier decades). The
 *   referent for extractiveness throughout is the standing market-default
 *   arrangement as this reading characterizes it at each point in time — not
 *   the cooperative/public-ownership alternative it displaced.
 *
 * KEY AGENTS:
 *   - incumbent_market_intermediaries: primary beneficiary of the later rationalization phase (institutional/arbitrage) — inherits, does not originate, the amnesia
 *   - financialized_asset_owners: passive beneficiary early, active funder of rationalization later (powerful/arbitrage)
 *   - market_economics_professoriate: institutional beneficiary whose own credentials now depend on defending the default (institutional/constrained) — a case of inherited identity-lock, not original design
 *   - displaced_worker_cooperatives_tradition: primary victim, structurally erased rather than actively suppressed in phase one, then locked out by phase-two rationalization (powerless/trapped)
 *   - municipal_ownership_advocates: secondary victim bearing an inherited and later reinforced burden of proof (moderate/constrained)
 *   - precarious_labor_force: diffuse victim of the naturalized allocation regime in its current, defended form (powerless/trapped)
 *   - economic_historians: analytical observer distinguishing the two phases empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.42).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.48).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default (Two-Stage Amnesia/Capture Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political_economy/ideology_studies/economic_history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '26049cdd-cb5a-4e97-b07b-b5f54d344e3b').
narrative_ontology:cs_kernel_codification('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', distributed).
narrative_ontology:cs_authority_grounding('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', extraction).
narrative_ontology:cs_interpretation_layer_present('26049cdd-cb5a-4e97-b07b-b5f54d344e3b').
narrative_ontology:cs_reading_relation('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', market_as_natural_default__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', foundational, amnesia_precedes_and_enables_capture).
narrative_ontology:cs_axiom_status(amnesia_precedes_and_enables_capture, holdable).
narrative_ontology:cs_axiom_grounding('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', amnesia_precedes_and_enables_capture, empirically_contingent).
narrative_ontology:cs_axiom('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', foundational, beneficiaries_inherit_rather_than_originate_the_default).
narrative_ontology:cs_axiom_status(beneficiaries_inherit_rather_than_originate_the_default, holdable).
narrative_ontology:cs_axiom_grounding('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', beneficiaries_inherit_rather_than_originate_the_default, empirically_contingent).
narrative_ontology:cs_reference_frame('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', postwar_technocratic_simplification).
narrative_ontology:cs_drift_state('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', post_1980_rationalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('26049cdd-cb5a-4e97-b07b-b5f54d344e3b', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financialized_asset_owners).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, displaced_worker_cooperatives_tradition).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, municipal_ownership_advocates).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, precarious_labor_force).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_allocation_is_the_default_baseline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Brokerages, financial trade associations, and large employers occupy the position vacated when mid-century coordination alternatives (sectoral bargaining boards, cooperative federations, public utility trusts) faded from institutional memory. They did not engineer that fading, but from the 1980s forward they fund think tanks, textbook chapters, and legal briefs that retroactively justify the market default as the only coherent baseline, converting an inherited gap into an actively defended position.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries, agenda_setter).

% Own the capital assets whose valuation depends on market allocation being treated as the unmarked, default mechanism rather than one option among several. Benefit passively from the amnesia in earlier decades and actively fund the post-1980s rationalization apparatus (financial economics chairs, ratings-agency doctrine, legal formalism around fiduciary duty) once profitable to do so.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financialized_asset_owners, beneficiary,
    powerful, biographical, arbitrage, global).

% Trained inside a curriculum from which mid-century institutional and cooperative economics had already been dropped (genuine curricular forgetting, not conspiracy) by the time they entered graduate programs in the 1970s-80s. Their careers, tenure cases, and journal placements now depend on treating market allocation as the theoretical starting point; the amnesia they inherited has become the premise they must defend to keep their own credentials coherent.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate, agenda_setter).

% The institutional descendants of pre-1930s mutual aid societies, producer cooperatives, and sectoral guilds have no organized voice today because the tradition's living carriers and archives thinned out across the mid-century gap. They cannot re-enter policy debate on equal footing because the vocabulary, case law, and financing mechanisms that once supported the alternative model no longer exist in usable form.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, displaced_worker_cooperatives_tradition, payer,
    powerless, generational, trapped, national).

% Advocate for public or municipal ownership of utilities, transit, and housing stock as a live historical alternative to market allocation. They must first re-establish that their proposal is not radical but restorative — a burden of proof that exists only because the earlier model's institutional memory lapsed and was not replaced with a neutral record, but with a rationalized market default.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, municipal_ownership_advocates, payer,
    moderate, biographical, constrained, regional).

% Workers in gig, contract, and at-will arrangements bear the practical costs of an allocation system now defended as natural and inevitable, foreclosing arguments for sectoral bargaining or guaranteed-hours regimes that were live policy options in the 1940s-50s and are now treated as fringe or utopian.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, precarious_labor_force, payer,
    powerless, immediate, trapped, national).

% Document the two-stage process: an initial, largely unplanned forgetting of coordination alternatives during the postwar consensus and Cold War curriculum shifts, followed by a distinct, traceable, well-funded rationalization campaign beginning in the 1980s. Their archival work is the primary evidence source for distinguishing genuine amnesia from later weaponized defense.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its first phase (1930s-1970s) the narrowing of taught and practiced allocation mechanisms to market-centric models reduced genuine coordination costs during postwar reconstruction and Cold War institution-building — fewer competing frameworks meant faster consensus in policy, education, and law.
% TRANSFER_FUNCTION: Moves the burden of proof and the practical returns of allocation decisions from displaced cooperative/public-ownership traditions and precarious workers toward incumbent market intermediaries, asset owners, and the professoriate that certifies the market frame as default — a transfer that was largely unintentional in its first decades and became structured and defended from the 1980s forward.
% ABSENT_VOICES: Descendants of the pre-1930s cooperative and mutual-aid tradition have no institutional standing to object; their archives are thin, their case law is stale, and the professional credentialing apparatus that would let them speak with authority (economics PhDs, legal scholarship, think-tank funding) was itself shaped by the amnesia they would need to contest.
% DISAPPEARANCE_RATIONALE: If the market-as-default framing vanished overnight, incumbent beneficiaries argue nothing would rearrange because market allocation is simply the coordinatively superior baseline that would re-emerge. Municipal ownership and cooperative advocates argue the world would rearrange substantially: policy proposals currently dismissed as radical (sectoral bargaining, public utility ownership, cooperative finance) would regain a level evidentiary footing, and the burden of proof in economic policy debate would shift.
% FOUNDING_PROBLEM: Coordinating postwar industrial reconstruction and Cold War-era economic policy quickly, using the simplest available theoretical apparatus, at a moment when institutional and cooperative economics traditions were already losing their academic and political carriers for unrelated reasons (McCarthy-era suspicion of cooperative/socialist-adjacent economics, wartime centralization of economic planning expertise into a narrower technocracy).
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside the beneficiary set (e.g., historians of the interwar cooperative movement and postwar curriculum studies) attest that the original coordination problem — rebuilding fast under Cold War pressure with a simplified toolkit — was resolved by the 1970s; what persists past that point is not the original problem but a defensive doctrine maintained by the professoriate and financial intermediaries who now depend on the market-default premise for their own institutional legitimacy.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts low (0.20) because the 1930s-1970s narrowing was not yet a mechanism of extraction — it was an information/curriculum contraction with modest distributive consequence. It rises through 1985-2000 as identifiable actors (financial intermediaries, asset owners, credentialed economists) begin actively defending the now-default frame against reintroduced alternatives, converting a passive gap into a maintained position with real distributive stakes. Suppression tracks a similar but more muted rise: the amnesia phase required little suppression (there was little organized alternative left to suppress), while the rationalization phase required active suppression of reintroduced cooperative/public-ownership proposals via burden-of-proof framing, legal doctrine, and funded scholarship. Theater ratio rises sharply after 1985 because a substantial share of the defensive apparatus (blue-ribbon commissions, textbook framing, 'free market' rhetoric in venues where the underlying economics is contested) is performative legitimation rather than functional coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent intermediaries, asset owners, professoriate) sit near the subsidized end of directionality because the current arrangement's default status directly reduces their argumentative and financial burden. Payers (displaced cooperative tradition, municipal ownership advocates, precarious labor) sit near the target end: the cooperative tradition is structurally trapped by loss of institutional memory rather than active blocking, while municipal advocates face an actively reinforced burden of proof, and precarious workers bear the practical downstream cost of a naturalized allocation regime. The professoriate stakeholder is deliberately marked constrained rather than arbitrage-mobile despite institutional power, because their credential-dependent identity lock (a specific instance of professional identity-lock) ties their material interest to defending the frame regardless of personal conviction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fast postwar coordination under a simplified toolkit) is genealogically dead by the 1970s per outside corroboration, yet the arrangement persists and intensifies afterward — this is the classic mismatch (status=dead, verdict=contested tilting toward world_rearranges) that flags capture rather than continued coordination. Classifying this as tangled_rope rather than snare or mountain matters: there was a real, if now-obsolete, coordination function in phase one, and phase two shows genuine active enforcement (rationalization apparatus) riding on top of it — both the coordination premise and the extraction premise are structurally present, which is precisely the tangled_rope gate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phase_boundary_precision,
    'Exactly when does the transition from genuine forgetting to active defensive rationalization occur, and is a single sharp boundary (c. 1980) historically defensible, or was the transition itself gradual and overlapping?',
    'Archival tracing of think-tank funding timelines, curriculum revision records, and legal-doctrine citation patterns (e.g., first appearance of market-default framing in appellate economic-regulation opinions) to locate when active defense first appears relative to when passive forgetting was already complete.',
    'A sharper, earlier boundary would shift more of the measured extraction into the defensive phase (supporting this reading''s shape); a more gradual, overlapping boundary would blur the distinction from beneficiary_maintained_reading and weaken the two-stage claim structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_boundary_precision, empirical, 'Whether the two-stage forgetting/rationalization boundary is a real historical discontinuity or an artifact of this reading''s framing.').

omega_variable(
    intentionality_of_original_narrowing,
    'Was the 1930s-1970s narrowing of taught coordination alternatives genuinely unplanned (driven by Cold War-era suspicion of cooperative/socialist-adjacent economics and wartime technocratic centralization), or did early, less visible beneficiary interests already shape which alternatives were dropped from curricula and policy discourse?',
    'Comparative institutional history: track funding sources and editorial boards of economics departments and textbook publishers in the 1940s-1960s for evidence of directed rather than incidental narrowing.',
    'If early narrowing shows directed beneficiary influence, this reading collapses toward beneficiary_maintained_reading and the low initial ε (0.20) would be understated; if narrowing is confirmed incidental, this reading''s two-stage structure is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentionality_of_original_narrowing, empirical, 'Whether Phase One amnesia was genuinely accidental or already beneficiary-shaped.').

omega_variable(
    counterfactual_alternative_viability,
    'Would the displaced cooperative/public-ownership coordination traditions have remained viable and effective at scale through the late 20th century absent the amnesia, or were they already declining for independent structural reasons (economies of scale, capital intensity shifts) that market default merely accelerated?',
    'Comparative case studies of jurisdictions (e.g., parts of Northern Europe, some U.S. municipal utility districts) that retained stronger cooperative/public-ownership traditions, assessing their relative economic performance and institutional durability.',
    'If alternatives were independently declining, the extraction attributable to the amnesia/capture mechanism specifically (versus general structural economic change) is smaller than the authored ε trajectory suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_alternative_viability, conceptual, 'Whether the counterfactual alternative was structurally viable, which bounds how much of current extraction is attributable to this constraint versus independent economic forces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 1930, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1930, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(mark_tr_t1950, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(mark_tr_t1970, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(mark_tr_t1985, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(mark_tr_t2000, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement(mark_tr_t2015, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(mark_tr_t2025, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(mark_be_t1930, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement(mark_be_t1950, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1950, 0.21).
narrative_ontology:measurement(mark_be_t1970, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1970, 0.24).
narrative_ontology:measurement(mark_be_t1985, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 1985, 0.31).
narrative_ontology:measurement(mark_be_t2000, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2000, 0.37).
narrative_ontology:measurement(mark_be_t2015, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement(mark_be_t2025, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1930, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1930, 0.15).
narrative_ontology:measurement(mark_su_t1950, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1950, 0.18).
narrative_ontology:measurement(mark_su_t1970, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1970, 0.22).
narrative_ontology:measurement(mark_su_t1985, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 1985, 0.34).
narrative_ontology:measurement(mark_su_t2000, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(mark_su_t2015, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(mark_su_t2025, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.15).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This story is the middle term in a three-member kernel family on market_as_natural_default. lapsed_alternative_reading claims innocent forgetting throughout with negligible ongoing extraction (closer to piton/mountain). beneficiary_maintained_reading claims continuous active capture from the outset (closer to snare). This hybrid_amnesia_reading claims a genealogical break: genuine low-extraction forgetting through the 1970s, followed by a distinct, traceable rationalization phase from the 1980s forward that beneficiaries did not originate but actively maintain. All three share the same underlying kernel (the market-as-default framing) but author structurally different ε trajectories and different classifications; they are linked here rather than merged because each has a stable, non-negotiable ε under its own reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
