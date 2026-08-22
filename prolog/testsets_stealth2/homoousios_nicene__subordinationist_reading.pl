% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios Under the Subordinationist Reading (Nicene Kernel)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested homoousios kernel of
 *   the Nicene settlement: the subordinationist reading, on which the creed's
 *   shared-substance language is compatible with the Son deriving his being
 *   from the Father — sharing divinity without equality — and on which
 *   scriptural authority, not accumulated conciliar gloss, adjudicates the
 *   creed's meaning. The standing arrangement under contest, and the fixed
 *   referent for epsilon, is the homoousios boundary as this reading
 *   administers it: a confession requirement that excludes the radical
 *   dissimilarity teaching (the Son as a creature) while licensing the
 *   hierarchical derivation-structure its holders read in scripture. Under
 *   this reading the boundary genuinely coordinates confession across the
 *   churches, and it simultaneously displaces the pro-Nicene equality party
 *   and the conciliar tradition's adjudicative authority — subordinationist
 *   communities gain admissibility, the strict-equality reading loses its
 *   exclusive claim, and the machinery of councils is demoted below
 *   scriptural exegesis. Epsilon is authored for THIS arrangement as the
 *   reading's own lights assess it; the metaphysical-equality and
 *   honorific-similarity readings are separate constraints (separate files),
 *   linked through network.affects_constraints. The claimed type and the
 *   metrics are independent authored facts: the type states what is
 *   structurally true from this seat; the metrics state what is descriptively
 *   true of the arrangement's operation, including its enforcement peak under
 *   Constantius and Valens and its repudiation at Constantinople.
 *
 * KEY AGENTS:
 *   - subordinationist_episcopal_coalition: agenda-setting administrator ([institutional]/[arbitrage]) — runs the boundary under this reading and collects its adjudicative authority
 *   - arian_semiarian_subordinationist_communities: primary beneficiary ([organized]/[constrained]) — their derivation-theology is admissible within the boundary
 *   - pro_nicene_equality_party: primary target ([organized]/[identity_locked]) — bears exclusion, deposition, and exile
 *   - conciliar_tradition_apparatus: secondary target ([institutional]/[constrained]) — its adjudicative role is demoted below scriptural exegesis
 *   - imperial_authority: coercive backer ([institutional]/[arbitrage]) — supplies enforcement when aligned, withdraws it on succession
 *   - anomean_dissimilarity_party: excluded voice ([organized]/[trapped]) — outside this boundary as much as the equality reading's
 *   - lay_congregations: excluded voice ([powerless]/[trapped]) — confess what the holding coalition prescribes
 *   - modern_historical_theology: analytical observer — sees all three readings as separate constraints over one kernel
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.55).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.6).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios Under the Subordinationist Reading (Nicene Kernel)").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, '7ad23340-d4c0-4422-b7e3-305d6b734471').
narrative_ontology:cs_kernel_codification('7ad23340-d4c0-4422-b7e3-305d6b734471', fixed_text).
narrative_ontology:cs_authority_grounding('7ad23340-d4c0-4422-b7e3-305d6b734471', lineage).
narrative_ontology:cs_interpretation_layer_present('7ad23340-d4c0-4422-b7e3-305d6b734471').
narrative_ontology:cs_reading_relation('7ad23340-d4c0-4422-b7e3-305d6b734471', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('7ad23340-d4c0-4422-b7e3-305d6b734471', homoousios_nicene__honorific_similarity_reading, coexists_with).
narrative_ontology:cs_axiom('7ad23340-d4c0-4422-b7e3-305d6b734471', foundational, son_derives_being_from_father).
narrative_ontology:cs_axiom_status(son_derives_being_from_father, holdable).
narrative_ontology:cs_axiom_grounding('7ad23340-d4c0-4422-b7e3-305d6b734471', son_derives_being_from_father, theological).
narrative_ontology:cs_axiom('7ad23340-d4c0-4422-b7e3-305d6b734471', foundational, scripture_supremacy_over_conciliar_tradition).
narrative_ontology:cs_axiom_status(scripture_supremacy_over_conciliar_tradition, holdable).
narrative_ontology:cs_axiom_grounding('7ad23340-d4c0-4422-b7e3-305d6b734471', scripture_supremacy_over_conciliar_tradition, conventional).
narrative_ontology:cs_reference_frame('7ad23340-d4c0-4422-b7e3-305d6b734471', scriptural_adjudication_settlement).
narrative_ontology:cs_drift_state('7ad23340-d4c0-4422-b7e3-305d6b734471', constantinople_381_repudiation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7ad23340-d4c0-4422-b7e3-305d6b734471', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, arian_semiarian_subordinationist_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_episcopal_coalition).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, pro_nicene_equality_party).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, conciliar_tradition_apparatus).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, scriptural_supremacy_over_conciliar_tradition).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, son_derives_being_from_father).
narrative_ontology:constraint_vindicates(homoousios_nicene__subordinationist_reading, subordination_compatible_with_homoousios).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bishops and synods that administer the creedal boundary under this reading through the middle decades of the fourth century: they convene councils, draft formulas, examine candidates for ordination, and — with imperial backing in the Constantius and Valens years — depose bishops who confess the Son's full equality. The work of adjudicating the creed's meaning flows to their networks, since on this reading scripture settles the term's sense and their exegesis administers the settlement. Their position is mobile in a specific way: the coalition repeatedly shifted between formulas (same substance, like in substance, like) as political conditions moved, which is exactly what their opponents held against them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, subordinationist_episcopal_coalition, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(homoousios_nicene__subordinationist_reading, subordinationist_episcopal_coalition, beneficiary).

% The congregations and teachers — Eusebian moderates, the like-in-substance middle party, the like-party, later the Gothic churches — whose teaching that the Son derives his being from the Father is admissible inside the boundary on this reading. They can confess the Nicene term while keeping the hierarchical derivation-structure they read in scripture. Leaving the boundary means schism from the imperial church; abandoning the derivation-structure means surrendering the theology that constitutes them; so they live inside the terms the boundary sets.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, arian_semiarian_subordinationist_communities, beneficiary,
    organized, generational, constrained, continental).

% The bishops and theologians who hold that the Nicene term secures the Son's full equality with the Father — Athanasius, Hilary, later the Cappadocians. Under this reading of the boundary their confession is ruled out of the creed's authoritative sense; during the middle decades their bishops are deposed and exiled, their sees filled by the holding coalition, and their appeals to the councils' accumulated rulings are answered with scripture. Their commitment is constitutive: Athanasius's repeated exiles rather than a formula shift is the pattern of what leaving would cost them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, pro_nicene_equality_party, payer,
    organized, generational, identity_locked, continental).

% The standing machinery of councils, synodal letters, and episcopal consensus through which the churches had settled disputed doctrine. Under this reading its rulings are demoted: the creed's meaning is adjudicated by scripture directly, so the accumulated gloss loses its binding force and the apparatus loses the authority that made its settlements hold. It cannot abandon the conciliar method without dissolving itself, so it bears the demotion from inside its own practice.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, conciliar_tradition_apparatus, payer,
    institutional, generational, constrained, continental).

% The emperors who supplied the coercive instrument the boundary's enforcement ran on in this period — Constantius II and Valens above all: exiling pro-Nicene bishops, convening the councils that imposed like-formulas, installing compliant sees. Their stake is political: doctrinal uniformity as stability. Each succession moved the boundary — Constantine imposed the term, Constantius enforced a subordinationist sense, Julian suspended enforcement entirely, Theodosius reversed the settlement — so their position shifts with the succession rather than with the theology.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, imperial_authority, agenda_setter,
    institutional, biographical, arbitrage, continental).

% The radical wing descended from Arius's stronger claims — Aetius, Eunomius and their circles — who hold the Son is unlike the Father in essence. The boundary under this reading excludes them as firmly as the equality reading does: a shared-substance term, however read, will not carry their teaching. They would argue the middle position is incoherent, but they sit outside the negotiation this reading conducts; their exclusion is one of the things the boundary's coordination is for.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, anomean_dissimilarity_party, excluded,
    organized, biographical, trapped, regional).

% The baptized majority, who receive the creed's sense from whichever episcopal coalition holds their city and bear the disruptions of its shifts: deposed bishops, divided congregations, rebaptism controversies, formulas changed between one council and the next. They have no seat in the adjudication; their recourse is the patronage of whichever bishop will champion them, which is to say their collective weight is exercised only through the very seats that adjudicate over them.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, lay_congregations, excluded,
    powerless, biographical, trapped, continental).

% The academic discipline that reconstructs the fourth-century contest from council acts, letters, and creedal variants. It stands outside the confession and sees the full structure: one persisting creedal text, three live readings of it, each with its own parties and its own costs — and it can name what the fourth-century parties could not, that they were arguing over the sense of a single term.
narrative_ontology:constraint_stakeholder(homoousios_nicene__subordinationist_reading, modern_historical_theology, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(homoousios_nicene__subordinationist_reading, subordinationist_episcopal_coalition).
narrative_ontology:fixing_cost_class(homoousios_nicene__subordinationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single creedal boundary on the Son's relation to the Father, binding scattered churches against the radical dissimilarity teaching (the Son as a creature) while leaving the Son's derivation-structure to be settled by scriptural exegesis rather than accumulated conciliar gloss.
% TRANSFER_FUNCTION: Moves adjudicative authority and doctrinal legitimacy: from the pro-Nicene conciliar line to scriptural adjudication administered through the subordinationist episcopal networks; the strict-equality reading's exclusive claim on the creed's authoritative sense is the principal thing transferred.
% ABSENT_VOICES: The Anomean dissimilarity party, excluded by this boundary as firmly as by the equality reading, would argue the middle position is incoherent; the lay congregations whose confession is adjudicated over their heads would object to bearing the costs of formula shifts without a seat; and during the subordinationist ascendancy the pro-Nicene bishops were themselves excluded from the adjudicating councils that condemned them.
% DISAPPEARANCE_RATIONALE: If this reading of the boundary vanished overnight, the fourth-century settlement rearranges: the Homoian episcopal majority loses its confession and its councils lose their object, the Gothic churches lose the doctrinal charter under which they received Christianity, the pro-Nicene party's fifty-year contest becomes retroactively unnecessary, and Constantinople's anathemas condemn a position no one holds. The equality reading's codification was built against this reading; remove the reading and the codification's history has no adversary.
% FOUNDING_PROBLEM: The Arian crisis: whether the Son is truly divine or a creature — the boundary was constructed at Nicaea to exclude the teaching that 'there was when he was not.'
% FOUNDING_PROBLEM_CORROBORATION: The pro-Nicene party — outside this reading's beneficiary set — attests the founding problem: Athanasius's Orations Against the Arians and Hilary's De Trinitate confirm the crisis was the denial of the Son's true divinity, even while they dispute this reading's solution. Constantine's letters and the Nicaean acta, sources outside every theological party, corroborate that the boundary was constructed against the teaching that the Son was not.
narrative_ontology:disappearance_verdict(homoousios_nicene__subordinationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(homoousios_nicene__subordinationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_nicene__subordinationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(homoousios_nicene__subordinationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(homoousios_nicene__subordinationist_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the arrangement genuinely solves a coordination problem — one creedal boundary excluding the teaching that the Son is a creature, which every party including the pro-Nicene party acknowledged needed excluding — while simultaneously displacing real authority: the strict-equality reading is ruled out of the creed's authoritative sense and the conciliar apparatus's rulings are demoted below scriptural exegesis. That combination — real coordination plus asymmetric authority displacement under active enforcement — is what the claimed type asserts, independently of the metrics. Suppression (0.60) is the raw structural fact that the boundary never held itself: it required councils, depositions, and exile at every phase, and the suppression_requirement series shows the requirement persisting even after Constantinople dismantled the enforcement capacity — the settlement was held by coercion, and the gap between requirement and remaining capacity is what ended it. Suppression is authored as an unscaled structural property; only extractiveness is scaled by directionality and scope in the engine's computation. Theater (0.35) rises across the interval because an increasing share of boundary-maintenance became staged: councils convened to ratify predetermined formulas, Ariminum's engineered unanimity, the ritual condemnation of positions already settled. Accessibility collapse (0.45): the boundary under this reading collapses some alternatives — radical dissimilarity is excluded as firmly as under the equality reading, and the strict-equality reading loses the boundary's authoritative sense — but the field stays contestable, with three live readings persisting across the whole interval, so alternatives do not fully collapse. Resistance (0.70) is the fifty-year pro-Nicene resistance: five exiles of Athanasius, the Cappadocian consolidation, the eventual reversal at Constantinople. The identity-coordination framing carries a known gaming risk — 'scriptural fidelity' can cover authority displacement — and the check is whether extraction concentrates on the powerless at large scope: here it does not; the principal cost-bearers are organized and institutional seats (the equality party, the conciliar apparatus), while the laity's burden is disruption mediated through episcopal patronage rather than direct extraction. Coalition power for the powerless seat runs through exactly that patronage channel, which is why it never operated independently. The receipt surface: the displaced adjudicative authority demonstrably accrues to the coalition seat — it administers the settlement and collects its authority — which is why gain_flow names that seat rather than the communities, who receive admissibility but not the authority itself; fixing_cost is prohibitive because displacing this instantiation cost the equality party five decades and five exiles and succeeded only when an emperor aligned with them.
 *
 * PERSPECTIVAL GAP:
 *   From the coalition's seat the arrangement is scriptural fidelity: the creed says what scripture says, no more. From the pro-Nicene seat the same boundary operates as dispossession — their confession ruled out of the creed's meaning, their bishops exiled, their appeals to conciliar precedent answered with a private-exegetical court of appeal. From the conciliar apparatus's seat it is demotion: the method by which the churches had settled doctrine is subordinated to whatever exegesis the holding network produces. The identity-lock on the pro-Nicene seat is religious and relational: the equality confession is constitutive of who they are, so exit is not a priced option but a self-betrayal — Athanasius contra mundum is the paradigm — and if that fusion broke, the seat's effective extraction would drop sharply as formula-shifting became available to them as it was to the coalition. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The subordinationist communities hold beneficiary positions: the boundary subsidizes their admissibility, so their directionality sits near the beneficiary end. The coalition is both administrator and collector — agenda-setter with a beneficiary secondary role — giving low directionality with enforcement costs layered on. The pro-Nicene party and the conciliar apparatus hold target positions: the boundary extracts their admissibility and their authority respectively, and the party's directionality is amplified toward the full-target end by its identity-locked exit, while the apparatus is trapped by its own method. The imperial seat is structurally ambivalent: it enforces whichever reading it aligns with and collects political stability rather than doctrinal rents, placing it nearer the symmetric middle. Scope is continental: verifying doctrinal compliance across the Mediterranean world is hard, which the engine's scope scaling registers as a modest amplification of effective extraction on the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — excluding the teaching that the Son is a creature — was real at the boundary's construction and is corroborated from outside the beneficiary set; whether it remains solved by THIS reading of the boundary is precisely what the equality party denies, so the R5 status is contested rather than dead. The classification keeps both faces visible: flattening the arrangement into pure extraction would erase the genuine coordination against dissimilarity that all parties, including the pro-Nicene party, acknowledged the boundary performs; flattening it into pure coordination would erase the authority displacement that the pro-Nicene party and the conciliar apparatus actually bore. The mandatrophy question for this reading turns on the repudiation: after Constantinople 381 the imperial instantiation is dead, and what persists in the diminished communities — and in the modern scholarly recovery of the reading — is part function, part performance, a drift the theater series documents. The mismatch check (contested founding status against a world_rearranges verdict) is the honest state here: the arrangement's disappearance would rearrange the doctrinal world, and the parties genuinely dispute whether its founding problem is still live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this subordinationist instantiation a faithful reading of the homoousios kernel, or does the kernel''s meaning settle with the metaphysical-equality or honorific-similarity sibling readings?',
    'The institutional adjudication occurred at Constantinople 381, where the equality reading was codified and this reading''s imperial settlement repudiated; the conceptual dispute persists in academic theology, where the sibling readings are reconstructed as separate constraints over the same text.',
    'If the equality sibling is right, this constraint''s geometry inverts: subordinationist communities become the cost-bearers of the enforced equality boundary and the pro-Nicene party becomes the beneficiary; if the honorific sibling is right, this reading''s extraction thins toward diplomatic ambiguity and its victim set largely empties.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame omega: this constraint is one reading of kernel homoousios_nicene; sibling readings would restructure the beneficiary and victim geometry.').

omega_variable(
    subordination_mode_ambiguity,
    'Is the subordination this reading licenses functional (order and role with being shared) or ontological (the Son''s being itself derived from the Father''s)?',
    'Close reading of the subordinationist corpus separating being-language (Eunomian and Homoian derivation claims) from order-language (Homoiousian and Origenist hierarchy-of-origin claims).',
    'A functional-only construal converges toward the equality sibling''s metaphysics and would soften the foreclosure between the readings; a full ontological construal hardens it and fixes the beneficiary/victim geometry as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_mode_ambiguity, conceptual, 'The reading''s own internal ambiguity between functional and ontological subordination.').

omega_variable(
    enforcement_intrinsicness,
    'Was the arrangement''s extractive operation intrinsic to the reading''s structure, or an artifact of Constantius''s and Valens''s coercive backing?',
    'Compare the reading''s operation under coercive conditions (353-378) with its operation under non-coercive conditions (the Eusebian 330s; the Gothic communities after 381).',
    'If artifact, the instantiation as such sits closer to pure coordination and the enforcement history should not count against the reading''s structure; if intrinsic, the extraction is structural and persists wherever the reading administers a boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_intrinsicness, empirical, 'Whether the extraction was an imperial artifact or structural to the reading itself.').

omega_variable(
    victim_seat_identity,
    'Is the party bearing this reading''s costs the pro-Nicene theological community as such, or the conciliar adjudicative method as such (which the equality reading happened to wield)?',
    'Trace whether the subordinationist corpus attacks the equality confession itself or the binding force of accumulated conciliar gloss as an adjudicative method.',
    'If the method is the target, the displacement is aimed at an institutional structure rather than a community, and the stakeholder geometry and coalition analysis change accordingly; if the community is the target, the authored geometry stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_seat_identity, conceptual, 'Which seat — theological community or adjudicative method — is the reading''s actual cost-bearer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, homoousios_nicene__subordinationist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(homo_tr_t0, observed).
narrative_ontology:measurement(homo_tr_t11, homoousios_nicene__subordinationist_reading, theater_ratio, 11, 0.18).
narrative_ontology:measurement_basis(homo_tr_t11, observed).
narrative_ontology:measurement(homo_tr_t22, homoousios_nicene__subordinationist_reading, theater_ratio, 22, 0.22).
narrative_ontology:measurement_basis(homo_tr_t22, observed).
narrative_ontology:measurement(homo_tr_t34, homoousios_nicene__subordinationist_reading, theater_ratio, 34, 0.28).
narrative_ontology:measurement_basis(homo_tr_t34, observed).
narrative_ontology:measurement(homo_tr_t45, homoousios_nicene__subordinationist_reading, theater_ratio, 45, 0.32).
narrative_ontology:measurement_basis(homo_tr_t45, observed).
narrative_ontology:measurement(homo_tr_t56, homoousios_nicene__subordinationist_reading, theater_ratio, 56, 0.35).
narrative_ontology:measurement_basis(homo_tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, homoousios_nicene__subordinationist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(homo_be_t0, observed).
narrative_ontology:measurement(homo_be_t11, homoousios_nicene__subordinationist_reading, base_extractiveness, 11, 0.38).
narrative_ontology:measurement_basis(homo_be_t11, observed).
narrative_ontology:measurement(homo_be_t22, homoousios_nicene__subordinationist_reading, base_extractiveness, 22, 0.48).
narrative_ontology:measurement_basis(homo_be_t22, observed).
narrative_ontology:measurement(homo_be_t34, homoousios_nicene__subordinationist_reading, base_extractiveness, 34, 0.62).
narrative_ontology:measurement_basis(homo_be_t34, observed).
narrative_ontology:measurement(homo_be_t45, homoousios_nicene__subordinationist_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement_basis(homo_be_t45, observed).
narrative_ontology:measurement(homo_be_t56, homoousios_nicene__subordinationist_reading, base_extractiveness, 56, 0.55).
narrative_ontology:measurement_basis(homo_be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, homoousios_nicene__subordinationist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(homo_su_t0, observed).
narrative_ontology:measurement(homo_su_t11, homoousios_nicene__subordinationist_reading, suppression_requirement, 11, 0.45).
narrative_ontology:measurement_basis(homo_su_t11, observed).
narrative_ontology:measurement(homo_su_t22, homoousios_nicene__subordinationist_reading, suppression_requirement, 22, 0.55).
narrative_ontology:measurement_basis(homo_su_t22, observed).
narrative_ontology:measurement(homo_su_t34, homoousios_nicene__subordinationist_reading, suppression_requirement, 34, 0.75).
narrative_ontology:measurement_basis(homo_su_t34, observed).
narrative_ontology:measurement(homo_su_t45, homoousios_nicene__subordinationist_reading, suppression_requirement, 45, 0.72).
narrative_ontology:measurement_basis(homo_su_t45, observed).
narrative_ontology:measurement(homo_su_t56, homoousios_nicene__subordinationist_reading, suppression_requirement, 56, 0.6).
narrative_ontology:measurement_basis(homo_su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Nicene homoousios' covers three structurally distinct claims and decomposes into a three-story family per the epsilon-invariance principle: this file instantiates the subordinationist reading (epsilon authored at 0.55 — real coordination against dissimilarity with substantial authority displacement); the metaphysical-equality sibling (homoousios_nicene__metaphysical_equality_reading) authors epsilon for the enforced equality boundary, whose cost-bearers are the subordinationist communities — the geometry inverts between the two files; the honorific-similarity sibling (homoousios_nicene__honorific_similarity_reading) authors epsilon for the diplomatic-blur arrangement, where extraction thins toward interpretive ambiguity and the victim set largely empties. Historical direction of influence runs honorific (the Eusebian compromise) to subordinationist (the middle-party settlement) to equality (the Constantinopolitan codification that repudiates both); each file links its siblings through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
