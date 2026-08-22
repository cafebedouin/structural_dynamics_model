% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: The Winnability Taboo: Rhetorical Contraction over Persisting Operational Planning
 *   domain: strategic studies / nuclear deterrence / international relations
 *
 * SUMMARY:
 *   After 1945 the question of whether a war could be won split into two
 *   layers that moved in opposite directions. In public discourse the
 *   question contracted: across administrations of both parties, saying that
 *   a nuclear war might be winnable became professionally and politically
 *   unsayable, culminating in ritual declarations that such a war 'cannot be
 *   won and must never be fought.' In classified space the question never
 *   closed: limited-use plans, escalation ladders, and counterforce target
 *   packages were designed, war-gamed, and refined continuously across the
 *   entire interval. This story authors the arrangement that holds the two
 *   layers apart — a rhetorical boundary maintained by career sanction,
 *   editorial gatekeeping, and political penalty, which stabilizes declared
 *   doctrine while removing the planning it coexists with from democratic
 *   contest. The claim/metric relationship is deliberate: claimed_type is
 *   authored from the structural reading (a genuine coordination function
 *   plus asymmetric extraction under active enforcement), while the metrics
 *   are authored from the arrangement's observed operation — the engine
 *   measures any divergence. KEY AGENTS (by structural relationship): -
 *   strategic_planners (institutional / identity_locked): primary beneficiary
 *   — collects insulation of operational planning from public contest;
 *   administers the operational layer the public rhetoric never touches -
 *   political_executives (institutional / mobile): secondary beneficiary —
 *   declare unthinkability publicly at no electoral cost while retaining and
 *   periodically expanding the classified option space -
 *   democratic_oversight_bodies (organized / trapped): primary victim — hold
 *   formal oversight powers over an object their public vocabulary declares
 *   settled - electorate (powerless / trapped): victim — cedes the
 *   winnability question from electoral contest entirely -
 *   civic_anti_nuclear_movements (organized / constrained): victim and
 *   contester — their strongest arguments are ruled out of sayable bounds -
 *   independent_scholars (moderate / constrained): excluded voice — study the
 *   operational layer only through documents the beneficiaries choose to
 *   declassify - deterrence_historians (analytical / analytical): analytical
 *   observer — sees both layers at once in the declassified record
 *
 * KEY AGENTS:
 *   - strategic_planners: primary beneficiary (institutional/identity_locked) — operational flexibility without public accountability
 *   - political_executives: secondary beneficiary (institutional/mobile) — public unthinkability at no cost, private options retained
 *   - democratic_oversight_bodies: primary victim (organized/trapped) — oversight of a non-deliberable object
 *   - electorate: victim (powerless/trapped) — topic removed from electoral contest
 *   - civic_anti_nuclear_movements: victim (organized/constrained) — core claims ruled unsayable
 *   - independent_scholars: excluded voice (moderate/constrained) — access runs through beneficiary-curated disclosure
 *   - deterrence_historians: analytical observer (analytical/analytical) — sees both layers in the declassified record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.66).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.6).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.66).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "The Winnability Taboo: Rhetorical Contraction over Persisting Operational Planning").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic studies / nuclear deterrence / international relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '608daa8b-832b-4a63-bc93-99cd78322696').
narrative_ontology:cs_kernel_codification('608daa8b-832b-4a63-bc93-99cd78322696', distributed).
narrative_ontology:cs_authority_grounding('608daa8b-832b-4a63-bc93-99cd78322696', extraction).
narrative_ontology:cs_interpretation_layer_present('608daa8b-832b-4a63-bc93-99cd78322696').
narrative_ontology:cs_reading_relation('608daa8b-832b-4a63-bc93-99cd78322696', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('608daa8b-832b-4a63-bc93-99cd78322696', war_winnability_post_1945__countervailing_thinkable, influences).
narrative_ontology:cs_axiom('608daa8b-832b-4a63-bc93-99cd78322696', foundational, discursive_contraction_does_not_bind_operational_planning).
narrative_ontology:cs_axiom_status(discursive_contraction_does_not_bind_operational_planning, holdable).
narrative_ontology:cs_axiom_grounding('608daa8b-832b-4a63-bc93-99cd78322696', discursive_contraction_does_not_bind_operational_planning, empirically_contingent).
narrative_ontology:cs_axiom('608daa8b-832b-4a63-bc93-99cd78322696', foundational, public_unthinkability_rhetoric_shields_classified_planning).
narrative_ontology:cs_axiom_status(public_unthinkability_rhetoric_shields_classified_planning, holdable).
narrative_ontology:cs_axiom_grounding('608daa8b-832b-4a63-bc93-99cd78322696', public_unthinkability_rhetoric_shields_classified_planning, empirically_contingent).
narrative_ontology:cs_reference_frame('608daa8b-832b-4a63-bc93-99cd78322696', post_1945_open_winnability_discourse).
narrative_ontology:cs_drift_state('608daa8b-832b-4a63-bc93-99cd78322696', contemporary_taboo_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('608daa8b-832b-4a63-bc93-99cd78322696', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, political_executives).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_bodies).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, electorate).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, civic_anti_nuclear_movements).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, independent_scholars).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, nuclear_unthinkability_doctrine).
narrative_ontology:constraint_vindicates(war_winnability_post_1945__rhetorical_contraction, nuclear_use_taboo_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and maintain war plans for constrained nuclear use — target packages, escalation ladders, limited-option branches — inside classification regimes the public rhetoric never touches. Speaking publicly about what they plan would end careers and invite political sanction; their professional identity as responsible custodians is built on not saying in public what they do in private. They administer the operational layer and collect its insulation from contest. Exit would mean abandoning the profession that formed them.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter).

% Presidents and defense secretaries declare in public that nuclear war cannot be won and must never be fought, while retaining — and periodically expanding — the classified option space their predecessors built. The public position costs them nothing electorally and buys alliance confidence; the private position preserves flexibility. They leave office on schedule; the arrangement outlasts them and their benefit ends with their tenure.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, political_executives, beneficiary,
    institutional, biographical, mobile, national).

% Legislative committees, audit institutions, and treaty-review bodies charged with overseeing nuclear posture. Their formal powers are extensive, but the object of oversight — what the plans actually assume about winning — sits behind classification and behind a public vocabulary that treats the question as settled. Hearings proceed on delivery systems and budgets while the planning assumptions themselves are not deliberable in public session. They cannot abandon the duty the boundary empties.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight_bodies, payer,
    organized, generational, trapped, national).

% Voters in democracies that maintain the plans. The question of whether a nuclear war could be won is absent from electoral competition: no party campaigns on it, no mandate forms around it, no ballot offers it. What reaches them is the assurance that the question does not arise. Their only channel of influence runs through the organized seats above them.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, electorate, payer,
    powerless, biographical, trapped, national).

% Mass movements — freeze campaigns, unilateralist and abolitionist organizations — whose core claims require saying out loud that planning for nuclear war-fighting exists and implies usability. The public vocabulary of unthinkability is used to dismiss them as alarmist, and their strongest arguments are ruled out of mainstream bounds. They persist at the margins, shifting toward disarmament framings that remain sayable, but cannot exit the discourse their object sits inside.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, civic_anti_nuclear_movements, payer,
    organized, generational, constrained, continental).

% Academics and journalists outside the clearance perimeter who study nuclear strategy from open sources. They cannot verify planning claims, and publishing arguments that treat winnability as live carries professional cost; several built careers on the boundary's edge. Their access to the operational layer runs entirely through documents the planning community itself chooses to declassify.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, independent_scholars, excluded,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(war_winnability_post_1945__rhetorical_contraction, independent_scholars, payer).

% Work from the declassified record — plan histories, national security council memoranda, war-game archives — where both layers are visible at once: the public doctrine of unthinkability and the continuous refinement of limited-use plans. They collect nothing from the arrangement and bear none of its costs; they can name the gap because their position is outside it.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, deterrence_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates elite and public discourse on nuclear weapons around a shared boundary of what may be said about using them. It stabilizes alliance and adversary signaling by keeping declared doctrine consistent across administrations, prevents electoral demand for usable-war options from forming, and maintains a common standard of responsible strategic speech whose violation carries recognizable sanction.
% TRANSFER_FUNCTION: Moves deliberative access and accountability from the democratic public to the strategic planning community: the public cedes the winnability question from contestable politics; planners receive a protected operational space in which limited-use planning continues without rhetorical contest, funded and expanded across administrations of both parties.
% ABSENT_VOICES: Independent scholars and journalists outside the clearance perimeter would contest planning assumptions if they could see them; legislators without access must vote on posture without the object of oversight; adversary planners read Western unthinkability rhetoric without a Western public debate to check it against. They are outside the classified conversation, and their objections enter only pre-filtered as irresponsible winnability talk.
% DISAPPEARANCE_RATIONALE: If the taboo vanished overnight, winnability would return to electoral contest: parties would campaign on usable-war or no-planning platforms, alliance signaling would wobble as declared doctrine became politically contingent, oversight committees would subpoena planning assumptions, and the planners' insulated space would close. The operational plans themselves would persist, but the arrangement that keeps them beyond accountability would not — the world this arrangement organizes would rearrange around open contest.
% FOUNDING_PROBLEM: After 1945 the problem was normalization: how to possess and plan around weapons of civilization-destroying power without making nuclear war-fighting a sayable, ordinary instrument of policy. The arrangement was built to keep the question of winning out of public speech so that possession would not slide into use.
% FOUNDING_PROBLEM_CORROBORATION: The planning community and allied governments attest the founding problem is live, citing deterrence's continuing need for the unthinkability frame. Corroboration from outside the benefiting parties: declassification-based historians (the National Security Archive lineage) attest that the insulation function grew as the anti-normalization urgency receded; the anti-nuclear movement attests the arrangement now blocks the deliberation it was built to protect; independent taboo scholarship attests the norm's real but partially displaced function. No source outside the beneficiary set attests that the founding problem remains the arrangement's primary operative function — that attestation gap is itself signal.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: the arrangement transfers deliberative control over the winnability question from the public to the planning community — a substantial extraction — but a genuine coordination function (consistent declared doctrine, anti-normalization of war-fighting) caps it below coercive maxima. Suppression 0.60: authored as the raw structural property it is, unscaled by power or scope — the enforcement is discursive and professional (career sanction, editorial gatekeeping, political penalty) rather than legal, which is why it sits below statutory-coercion levels while still being the arrangement's load-bearing mechanism. Theater_ratio 0.42: the public layer is heavily performative — ritual declarations, doctrinal reaffirmations, and denunciations of 'winnability' talk defend the boundary rather than performing anti-normalization work, and they coexist with intensive non-theatrical planning underneath; the operational layer itself is not theatrical. Accessibility_collapse 0.55: alternatives — open deliberation on winnability — collapse substantially in mainstream electoral and media space but persist in specialist journals, war colleges, and classified channels; the collapse is venue-specific, not total. Resistance 0.60: the freeze movement, abolitionist campaigns, congressional oversight pushes, and dissident strategists mounted sustained contest that the arrangement absorbed rather than never faced. Measurement series run on one shared grid (9 points, 1945–2025): extractiveness and suppression rise together through the Cold War to a 1985 peak (freeze-era contest met by peak enforcement while limited-option planning expanded), decay after 1991 as enforcement machinery lost salience, then partially rebuild with post-2010 modernization and renewed great-power competition. The shape is rise–peak–decay–partial-rebuild, not cyclical; no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the planner seat the arrangement is professional discipline: responsible custodians do not say in public what they prepare in private, and the boundary protects doctrine from political weather. From the oversight seat the same boundary is enforced silence about the object of oversight — committees hold hearings on delivery systems and budgets while the planning assumptions themselves are not deliberable. Same-power divergence: democratic_oversight_bodies and civic_anti_nuclear_movements both hold organized power, but oversight bodies are trapped (they cannot abandon the duty that the boundary empties), while movements are constrained (they can reframe toward abolition — a sayable position — but cannot exit the discourse their object sits inside). Inter-institutional: the executive both administers the public layer and benefits from the private one; the legislature pays without the access that would let it contest substance. The engine computes per-seat classifications from this structural data; the seat divergence is the measurement, not something the claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to directionality as follows. Strategic_planners: beneficiary with identity_locked exit — near the beneficiary end; the identity fusion (professional self-concept constituted by the responsible-custodian role) does not raise their extraction, it raises what the arrangement costs them if it ever breaks. Political_executives: beneficiary with mobile exit — low d; their benefit is partly reputational and ends with office, which damps it further. Democratic_oversight_bodies: victim, trapped — high d; the arrangement removes exactly what their function requires. Electorate: victim, powerless, trapped — nearest the full-target end; their coalition potential runs only through the organized seats, which is precisely why a discursive boundary is an effective extraction mechanism: it fragments coalition formation at the layer where coalitions form. Civic_anti_nuclear_movements: victim, constrained — high d, slightly damped by the reframing exit toward abolitionism. Independent_scholars: excluded/victim, constrained — high-moderate d; their access to the operational layer runs entirely through beneficiary-curated disclosure, so even their knowledge is rationed by the seats they would scrutinize. Deterrence_historians: observer, analytical — no extraction in either direction. Effective extraction is amplified for trapped victims at national scope (verification of what the plans assume is hardest for exactly the seats barred from seeing them) and damped for the mobile beneficiary; suppression is not scaled — the discursive enforcement is the same raw force for every seat that touches the boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two symmetric mislabels. A pure-coordination reading would erase the accountability extraction — treating the taboo as costless stabilization and the blocked oversight as an acceptable side effect. A pure-extraction reading would erase the genuine anti-normalization function that the historical non-use record partly vindicates — the taboo did keep war-fighting out of sayable politics during decades when it might have entered. The structural data holds both: coordination (signaling stability, boundary maintenance) and extraction (insulation for planners, blocked deliberation for publics) ride the same enforcement machinery. Mandatrophy: the founding problem — preventing public normalization of nuclear war-fighting — is contested as the arrangement's operative function; the accountability-insulation function grew as the original problem's post-Cold-War urgency receded, and the arrangement now does both. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (world_rearranges): no dead-mandate flag fires, but the contested status marks exactly the question omega stabilization_insulation_share carries. Fixing is prohibitive relative to benefit because opening winnability to electoral deliberation would unsettle alliance signaling and force disclosure choices whose costs fall on the same institutional seats that would have to authorize the fix — the cost-asymmetry that keeps the arrangement in place is authored in fixing_cost, independent of who receives the gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'Is the dual-layer structure real, or does one of the sibling readings capture the kernel instead — categorical unthinkability (no operational space persists to insulate) or plain thinkability (no rhetorical contraction worth modeling)?',
    'Adjudicate against the declassified planning record and discourse corpora: if planning output atrophied as public rhetoric contracted, the deterrence_unthinkable reading captures the structure; if public discourse never contracted, the countervailing_thinkable reading does.',
    'Under the deterrence_unthinkable reading this story''s extraction collapses — insulation of nothing is no benefit and the arrangement reduces to a discursive fact; under the countervailing_thinkable reading the taboo loses its coordination function and the arrangement drifts toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Which reading of the war_winnability_post_1945 kernel the dual-layer structure instantiates.').

omega_variable(
    declassified_record_selection_bias,
    'Can the operational layer be observed independently of the beneficiaries who curate its evidence?',
    'Triangulate across multiple declassification regimes, foreign archives, and leak records; compare planning tempo inferred from budgets, personnel, and test activity against document-based accounts.',
    'If the operational record is curated by the planning community itself, the measured gap between rhetorical contraction and operational persistence may be overstated and the extraction estimate falls; a record robust across independent sources raises confidence in the dual-layer reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declassified_record_selection_bias, empirical, 'Whether the operational layer''s evidence base is independent of the seats that benefit from it.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the taboo''s suppression structural (classification law, career gatekeeping, editorial norms) or internalized (strategists and publics have absorbed unthinkability so thoroughly that enforcement is self-executing)?',
    'Post-Cold-War enforcement trajectory: if measured suppression persisted while formal enforcement machinery decayed after 1991, the internalized share is large; if suppression fell in step with enforcement capacity, the structural share dominates.',
    'If largely internalized, the constraint outlives its enforcement apparatus — the post-1991 falling suppression series understates the arrangement''s durable force and the classification hardens; if structural, enforcement decay is genuine relaxation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the winnability taboo.').

omega_variable(
    stabilization_insulation_share,
    'How much of the taboo''s persistence is genuine deterrence stabilization (coordination benefit) versus accountability insulation (extraction)?',
    'Natural experiments where the public rhetoric lapsed — the 1984 ''cannot be won'' declaration, 2016 campaign loose talk — while deterrence held: if signaling stability survived rhetorical lapses, stabilization is a smaller share of the arrangement''s function than its defenders claim.',
    'A low stabilization share moves the arrangement toward pure extraction with a cover story; a high share vindicates the coordination component and caps effective extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stabilization_insulation_share, empirical, 'The split between the taboo''s stabilizing and insulating functions.').

omega_variable(
    scope_of_taboo_enforcement,
    'Is the rhetorical contraction a national (US/allied) arrangement or a global discursive structure — and does the fact that adversary planners do not share the taboo impair the coordination function?',
    'Comparative discourse analysis across nuclear and non-nuclear states'' public spheres; adversary doctrine publications are public and can be contrasted with Western public silence.',
    'If the taboo is one-sided, its signaling-coordination function is impaired (asymmetric disclosure) while its extraction concentrates on a single public — raising effective extraction for that seat; if reciprocated, coordination is stronger and extraction more diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_taboo_enforcement, empirical, 'Geographic and reciprocity scope of the rhetorical contraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t0, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(war__tr_t0, observed).
narrative_ontology:measurement(war__tr_t10, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(war__tr_t10, observed).
narrative_ontology:measurement(war__tr_t20, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(war__tr_t20, observed).
narrative_ontology:measurement(war__tr_t30, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 30, 0.35).
narrative_ontology:measurement_basis(war__tr_t30, observed).
narrative_ontology:measurement(war__tr_t40, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 40, 0.5).
narrative_ontology:measurement_basis(war__tr_t40, observed).
narrative_ontology:measurement(war__tr_t50, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 50, 0.45).
narrative_ontology:measurement_basis(war__tr_t50, observed).
narrative_ontology:measurement(war__tr_t60, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(war__tr_t60, observed).
narrative_ontology:measurement(war__tr_t70, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 70, 0.4).
narrative_ontology:measurement_basis(war__tr_t70, observed).
narrative_ontology:measurement(war__tr_t80, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 80, 0.42).
narrative_ontology:measurement_basis(war__tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t0, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(war__be_t0, observed).
narrative_ontology:measurement(war__be_t10, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(war__be_t10, observed).
narrative_ontology:measurement(war__be_t20, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(war__be_t20, observed).
narrative_ontology:measurement(war__be_t30, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(war__be_t30, observed).
narrative_ontology:measurement(war__be_t40, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(war__be_t40, observed).
narrative_ontology:measurement(war__be_t50, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 50, 0.6).
narrative_ontology:measurement_basis(war__be_t50, observed).
narrative_ontology:measurement(war__be_t60, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(war__be_t60, observed).
narrative_ontology:measurement(war__be_t70, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 70, 0.65).
narrative_ontology:measurement_basis(war__be_t70, observed).
narrative_ontology:measurement(war__be_t80, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 80, 0.66).
narrative_ontology:measurement_basis(war__be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t0, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(war__su_t0, observed).
narrative_ontology:measurement(war__su_t10, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 10, 0.28).
narrative_ontology:measurement_basis(war__su_t10, observed).
narrative_ontology:measurement(war__su_t20, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(war__su_t20, observed).
narrative_ontology:measurement(war__su_t30, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(war__su_t30, observed).
narrative_ontology:measurement(war__su_t40, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(war__su_t40, observed).
narrative_ontology:measurement(war__su_t50, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(war__su_t50, observed).
narrative_ontology:measurement(war__su_t60, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(war__su_t60, observed).
narrative_ontology:measurement(war__su_t70, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 70, 0.55).
narrative_ontology:measurement_basis(war__su_t70, observed).
narrative_ontology:measurement(war__su_t80, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 80, 0.6).
narrative_ontology:measurement_basis(war__su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, identity_coordination).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% The colloquial label 'winnability of war after 1945' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle: war_winnability_post_1945__deterrence_unthinkable (categorical unthinkability — planning for victory is incoherent), war_winnability_post_1945__countervailing_thinkable (limited victory achievable through counterforce), and this story, war_winnability_post_1945__rhetorical_contraction (public discourse contracted while classified planning persisted). Each carries its own epsilon, beneficiary structure, and classification: this reading's epsilon measures the accountability extraction of the taboo arrangement itself. The siblings are linked here because each is cited as evidence in contests over the others — unthinkability is invoked to defend the taboo's necessity; thinkability is invoked to expose the taboo's gap; the dual-layer record is invoked against both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
