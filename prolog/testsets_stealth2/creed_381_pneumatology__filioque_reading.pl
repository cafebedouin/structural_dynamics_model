% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_creed_381_pneumatology__filioque_reading, []).

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
 *   constraint_id: creed_381_pneumatology__filioque_reading
 *   human_readable: Filioque Clause Enforcement and Magisterial Clarification Authority (Roman Reading)
 *   domain: historical_theology/ecclesiastical_authority/commitment_systems
 *
 * SUMMARY:
 *   A single-word addition to a received ecumenical creed, enforced as a
 *   condition of communion, paired with an authority claim that a living
 *   magisterium may render explicit what the received text states implicitly.
 *   The arrangement began as a regional anti-subordinationist fortification
 *   in sixth-century Spain, was adopted by the Frankish imperial court as a
 *   stick against Constantinople, entered Roman liturgical use in 1014,
 *   became the standing doctrinal wall of the 1054 rupture, was enforced by
 *   arms at 1204 and by diplomacy at the attempted unions of 1274 and 1439,
 *   reached maximal juridical form under the 1870 centralization, and has
 *   since selectively relaxed in imposition while remaining normative for the
 *   Latin communion. Throughout, the same instrument performs two operations
 *   at once: it keeps one confession coherent across a civilization-scale
 *   communion, and it transfers decisions about the shared text from all the
 *   churches that received it to the one see that amended it. KEY AGENTS (by
 *   structural relationship): - papal_see: Primary beneficiary and
 *   agenda-setter (institutional / identity_locked) — administers the clause,
 *   collects the clarification-authority yield - eastern_patriarchates:
 *   Primary target (institutional / constrained) — bear the autonomy
 *   transfer, exit priced at schism - eastern_orthodox_monastics: Organized
 *   resistance core (organized / identity_locked) -
 *   eastern_catholic_uniate_churches: Dual-positioned (moderate /
 *   constrained) — communion protection received, clarification yield not -
 *   latin_western_episcopate: Secondary beneficiary (institutional /
 *   constrained) - latin_lay_faithful: Diffuse incidental beneficiaries
 *   (powerless / constrained) - oriental_orthodox_churches: Excluded
 *   non-consenting confessors of the same text (institutional / trapped) -
 *   ecumenical_dialogue_commissions: Analytical observer (institutional /
 *   analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.74).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.6).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Clause Enforcement and Magisterial Clarification Authority (Roman Reading)").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority/commitment_systems").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, 'a21bd145-13f8-4861-a8d9-deb135c06b6e').
narrative_ontology:cs_kernel_codification('a21bd145-13f8-4861-a8d9-deb135c06b6e', fixed_text).
narrative_ontology:cs_authority_grounding('a21bd145-13f8-4861-a8d9-deb135c06b6e', lineage).
narrative_ontology:cs_interpretation_layer_present('a21bd145-13f8-4861-a8d9-deb135c06b6e').
narrative_ontology:cs_reading_relation('a21bd145-13f8-4861-a8d9-deb135c06b6e', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('a21bd145-13f8-4861-a8d9-deb135c06b6e', creed_381_pneumatology__ecumenical_reunion_reading, influences).
narrative_ontology:cs_axiom('a21bd145-13f8-4861-a8d9-deb135c06b6e', foundational, spirit_proceeds_from_father_and_son).
narrative_ontology:cs_axiom_status(spirit_proceeds_from_father_and_son, holdable).
narrative_ontology:cs_axiom_grounding('a21bd145-13f8-4861-a8d9-deb135c06b6e', spirit_proceeds_from_father_and_son, theological).
narrative_ontology:cs_axiom('a21bd145-13f8-4861-a8d9-deb135c06b6e', foundational, magisterium_may_clarify_implicit_creeds).
narrative_ontology:cs_axiom_status(magisterium_may_clarify_implicit_creeds, holdable).
narrative_ontology:cs_axiom_grounding('a21bd145-13f8-4861-a8d9-deb135c06b6e', magisterium_may_clarify_implicit_creeds, conventional).
narrative_ontology:cs_reference_frame('a21bd145-13f8-4861-a8d9-deb135c06b6e', living_creeds_under_magisterial_clarification).
narrative_ontology:cs_drift_state('a21bd145-13f8-4861-a8d9-deb135c06b6e', post_1995_clarification_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a21bd145-13f8-4861-a8d9-deb135c06b6e', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_western_episcopate).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_patriarchates).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_orthodox_monastics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, latin_lay_faithful).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, eastern_catholic_uniate_churches).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_catholic_uniate_churches).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, double_procession_doctrine).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, magisterial_clarification_authority).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, unilateral_definition_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Latin communion's confession of faith: promulgated the clause as normative text, defines what counts as orthodox teaching on the Spirit's origin, and gates communion with Rome on acceptance of its doctrinal judgments. Collects the practical fruits of clarification authority — each successful clarification strengthens the precedent that the See may act on the shared creed without waiting for general consent. Its freedom to drop the clause is nominal only: having taught for centuries that the addition is legitimate, retracting it would implicate the same authority that issued it.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, papal_see, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Confesses and teaches the clause as part of the inherited creed; gains a uniformly worded confession across dioceses and languages and shares in the teaching authority that adjudicates disputed texts. Bears implementation costs (catechesis, liturgical books, discipline) but sets nothing: the text and its status arrive from Rome.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_western_episcopate, beneficiary,
    institutional, generational, constrained, global).

% Recite the clause weekly as fixed liturgical text; most never encounter the dispute over its origin or status. They receive whatever unity of confession the arrangement provides and bear no direct burden; leaving the arrangement means leaving the church altogether, so the practical choice set is acceptance.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, latin_lay_faithful, beneficiary,
    powerless, biographical, constrained, global).

% Guard the creed as received at Constantinople in 381 and treat its wording as settled by the council that issued it. Every extension of the clause's authority presses on their confessional autonomy: accepting it means conceding that one see may amend what all churches received together; refusing it costs communion with the West, an exchange they judged ruinous and refused in 1054 and again after the attempted reunions of 1274 and 1439.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_patriarchates, payer,
    institutional, civilizational, constrained, continental).

% Monastic federations and Athonite communities function as the memory and conscience of the Eastern confession: they supplied the resistance at Lyon and Florence, produced the theological literature against the addition, and treat uncompromising fidelity to the received text as constitutive of their vocation. Exit would mean abandoning the identity the resistance sustains.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_orthodox_monastics, payer,
    organized, generational, identity_locked, regional).

% Communities in communion with Rome (Melkite, Ukrainian, Ruthenian, Maronite and others) permitted to keep their Byzantine liturgies. They receive protection and recognition from communion with Rome while bearing the clause's confessional weight: their theologians largely prefer the Greek formulation of the Spirit's origin, yet their communion status depends on the See that promulgated the Latin wording. They collect none of the clarification authority the arrangement generates.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_catholic_uniate_churches, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, eastern_catholic_uniate_churches, beneficiary).

% Ancient non-Chalcedonian churches (Armenian, Coptic, Syriac, Ethiopian) that confess the creed of 381 within their own traditions. They were consulted by no party in the clause's adoption or its later defenses, and they sit outside the bilateral dialogue table where its status is currently negotiated; their objection to unilateral amendment of a shared text has no venue.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, oriental_orthodox_churches, excluded,
    institutional, civilizational, trapped, continental).

% Joint Catholic-Orthodox commissions (Balamand, Chieti and successors) study the clause's history and the vocabulary of procession, publish agreed statements, and formulate proposals under which the Latin and Greek formulations might coexist. They decide nothing: their outputs bind neither communion and enter force only if the administering authorities adopt them.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ecumenical_dialogue_commissions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__filioque_reading, papal_see).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__filioque_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one verbally identical confession across a communion spanning continents, languages, and centuries, and provides a standing mechanism for resolving ambiguities a fixed fourth-century text necessarily leaves open — the Greek and Latin vocabularies of the Spirit's origin did not map onto each other, and someone must adjudicate the mapping for a single communion to keep confessing together.
% TRANSFER_FUNCTION: Moves doctrinal-jurisdictional authority from the whole body of churches (consent of all) to the Roman see and its magisterium (decision of one); moves confessional compliance outward from Rome to every church in communion; each exercise of clarification authority transfers a further increment of autonomy from the local churches to the center.
% ABSENT_VOICES: The bishops of the East were absent from every act that made the clause binding — Toledo, Aachen, the Roman adoption of 1014 — and the oriental Orthodox are absent from today's negotiation table; the Greek vocabulary of procession had no interpreter present when the Latin wording was given normative force. Objection is recorded only from outside the arrangements that created the fact on the ground.
% DISAPPEARANCE_RATIONALE: If the clause and its enforcement vanished overnight, the oldest standing doctrinal wall between Rome and the East would fall: reunion negotiations would restructure around the remaining questions (the extent of primacy, unleavened bread, purgatory), Latin liturgical books would revert toward the received Greek-era text, Eastern Catholic communities would regain full confessional symmetry, and the precedent that a single see may amend the shared creed — the load-bearing precedent beneath the later magisterial edifice — would lose its founding instance.
% FOUNDING_PROBLEM: Two problems, layered: pastorally, sixth-century Visigothic Spain needed to fortify the Spirit's full divinity against surviving Arian subordinationism, and the addition served that fight; structurally, a universal communion inherited a creed whose two linguistic versions of the Spirit's origin did not say obviously the same thing, and the arrangement answers the standing need for an authority that can settle what the received text implies.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Eastern patriarchates and their theologians (Photius, Mark of Ephesus, the 1484 Constantinopolitan synod) attest the anti-Arian problem was real but died with Arianism and never justified acting without the churches that received the text; academic historians of the creeds independently corroborate the Toledo origin and the later imperial and juridical instrumentalization; the modern joint commissions attest the vocabulary problem is real but tractable by bilateral study. No attesting source outside the Latin communion maintains that the clarification mechanism required unilateral exercise.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(creed_381_pneumatology__filioque_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(creed_381_pneumatology__filioque_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because the arrangement's continuing yield is jurisdictional: every decade the clause stands as an enforced communion condition, the precedent that one see may settle the shared text compounds. Suppression (0.60) is structural throughout — communion discipline, anathema, and at three episodes armed or diplomatic coercion — never internalized belief management; the East's internal theological life stayed autonomous, so suppression operates at boundaries rather than minds, and the scalar is a raw structural property the engine does not scale. Theater (0.42) is real but sub-dominant: reciting the clause where no doctrinal question is live, and ceremonial insistence at union negotiations, are performances; but the boundary function remains genuinely operative, so performance stays below half of activity. The series run on one shared grid (589, 800, 1054, 1204, 1439, 1870, 2020) with every tracked metric authored at every point. The suppression trajectory is cyclical rather than monotonic: build-up to the 1204 coercive peak, partial relaxation during late-Byzantine union diplomacy, ratchet to maximum under 1870 centralization, decline in the ecumenical era — and each cycle closed with the constraint holding more than before, because each reconciliation attempt (Lyon, Florence) was priced in concessions that outlasted the reconciliations; the oscillation itself functioned as an extraction mechanism, not noise.
 *
 * PERSPECTIVAL GAP:
 *   From the papal seat the arrangement is the church's own living voice doing what a living voice must: the creed was always implicit, the See made it explicit, and the East's objection is to a clarity it resents. From the Eastern seats the same structure is jurisdictional capture in doctrinal dress: the question was never whether the Spirit's origin could be further specified but who owns the specifying. The Latin episcopate experiences a third thing — inherited text plus occasional directives from a center it does not control. The engine computes these divergences per seat from the structural data (power, exit, role); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place the papal see nearest the subsidy end (collects the arrangement's yield and administers its enforcement), the Latin episcopate low (shares uniformity, sets nothing), and the lay faithful low-mid (diffuse incidental benefit, no rents collected). Victim declarations place the Eastern patriarchates near the target end (bear the autonomy transfer, exit priced at schism) and the monastic communities at the extreme target end (identity-locked: the resistance constitutes the vocation, so exit is unthinkable from inside the frame). One override: the Eastern Catholic churches derive ambiguously from their dual payer/beneficiary declaration and would likely land near symmetric; their actual position is modestly target-side (0.55) because they receive protection and recognition but collect none of the clarification-authority yield, and they bear confessional assimilation pressure the Latin churches do not — the override is keyed to their unique moderate-power seat so it touches no other agent. Spatial scope amplifies: the arrangement operates at continental-to-global scale across linguistic lines, verification of confessional compliance is hard, and effective extraction scales up accordingly for the targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem's pastoral half (anti-Arian fortification) is dead — Arianism ceased to be a live force within the arrangement's own communion centuries ago — while the structural half (vocabulary adjudication) remains arguable, hence status contested. The classification prevents two mislabels: reading the arrangement as pure extraction erases the genuine coordination function (a communion of this size cannot hold without a common confession and some adjudication mechanism for its fixed text); reading it as pure coordination erases the unilateral-amendment extraction the East has paid for ten centuries. The tangled-rope structure holds both: enforcement is active, coordination is real, and the same instrument delivers both. The arrangement is not a piton: theater stays below half, the boundary function is operative, and what blocks revision is the administrator's identity-lock rather than mere inertia — the See could in principle drop the clause (Leo III refused to add it), but the cost of doing so now implicates the authority that issued it, so the cost class is prohibitive and the capture cell governs under either cost reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the filioque_reading of kernel creed_381_pneumatology; what do the sibling readings (monoprocession_reading, ecumenical_reunion_reading) change structurally, and where is the disagreement located?',
    'Read against the sibling stories: the disagreement sits in two elements — the metaphysics of the Spirit''s origin (one spirating principle or two) and the locus of authority over a received ecumenical text (magisterial clarification versus ecumenical consent).',
    'Under monoprocession_reading the beneficiary/victim structure inverts: the Eastern churches become the text''s guardians and the unilateral amender the breaching party, redistributing epsilon toward the West. Under ecumenical_reunion_reading enforcement demobilizes, gain_flow turns diffuse, and epsilon collapses toward the coordination floor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame record: one reading of a contested kernel; sibling deltas and disagreement location.').

omega_variable(
    procession_vocabulary_equivalence,
    'Do the Greek ekporeusis and Latin processio term-pairs admit a single referent, such that ''from the Father through the Son'' and ''from the Father and the Son'' assert the same relation of origin?',
    'Comparative philological-dogmatic study: the 1995 Vatican study on the Greek and Latin traditions regarding the procession of the Holy Spirit, the Chieti (2016) and successor commission documents, and independent patristics scholarship on the Cappadocian and Augustinian corpora.',
    'If the vocabularies converge on one referent, the doctrinal-content extraction largely evaporates and the residual constraint is purely jurisdictional (who may speak for the shared text); if they diverge, the clause asserts a materially different doctrine than the text it amended, and the doctrinal extraction is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procession_vocabulary_equivalence, empirical, 'Whether the procession-vocabulary dispute is translational or doctrinal.').

omega_variable(
    suppression_reach_boundary_vs_internal,
    'Does the arrangement''s suppressive force operate only at communion boundaries (intercommunion denial, anathema, coerced unions) or does it reach into the Eastern churches'' internal doctrinal life?',
    'Classify enforcement incidents across the interval by site: boundary events (1054, 1204, Lyon, Florence, Uniate discipline) versus internal-life interference (censorship of Eastern theological publication, pressure on Eastern Catholic synodal decisions).',
    'Boundary-only suppression caps the effective burden on the Eastern seats below what raw epsilon suggests, since their internal theological economy stayed autonomous; demonstrated internal reach raises effective suppression and pushes the payer seats'' computed classifications toward harder types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_reach_boundary_vs_internal, empirical, 'Site of enforcement: communion boundary versus internal doctrinal life.').

omega_variable(
    clarification_authority_necessity,
    'Is vesting clarification of the shared creed in a single magisterium structurally necessary for doctrinal coherence at communion scale, or is it one selectable mechanism among several (conciliar, synodal, reception-based)?',
    'Comparative institutional analysis: coherence outcomes under conciliar (pre-schism pentarchal), synodal (sobornost-model), and magisterial regimes facing comparable doctrinal ambiguities in a fixed authoritative text.',
    'If necessary, part of the measured extraction is irreducible coordination cost and the tangled-rope reading strengthens; if selectable, the centralization is a choice whose asymmetry is fully attributable, strengthening extraction-weighted classification of the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clarification_authority_necessity, conceptual, 'Necessity versus selectability of centralized clarification authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 589, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t589, creed_381_pneumatology__filioque_reading, theater_ratio, 589, 0.15).
narrative_ontology:measurement_basis(cree_tr_t589, observed).
narrative_ontology:measurement(cree_tr_t800, creed_381_pneumatology__filioque_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement_basis(cree_tr_t800, observed).
narrative_ontology:measurement(cree_tr_t1054, creed_381_pneumatology__filioque_reading, theater_ratio, 1054, 0.28).
narrative_ontology:measurement_basis(cree_tr_t1054, observed).
narrative_ontology:measurement(cree_tr_t1204, creed_381_pneumatology__filioque_reading, theater_ratio, 1204, 0.36).
narrative_ontology:measurement_basis(cree_tr_t1204, observed).
narrative_ontology:measurement(cree_tr_t1439, creed_381_pneumatology__filioque_reading, theater_ratio, 1439, 0.4).
narrative_ontology:measurement_basis(cree_tr_t1439, observed).
narrative_ontology:measurement(cree_tr_t1870, creed_381_pneumatology__filioque_reading, theater_ratio, 1870, 0.46).
narrative_ontology:measurement_basis(cree_tr_t1870, observed).
narrative_ontology:measurement(cree_tr_t2020, creed_381_pneumatology__filioque_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(cree_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t589, creed_381_pneumatology__filioque_reading, base_extractiveness, 589, 0.25).
narrative_ontology:measurement_basis(cree_be_t589, observed).
narrative_ontology:measurement(cree_be_t800, creed_381_pneumatology__filioque_reading, base_extractiveness, 800, 0.45).
narrative_ontology:measurement_basis(cree_be_t800, observed).
narrative_ontology:measurement(cree_be_t1054, creed_381_pneumatology__filioque_reading, base_extractiveness, 1054, 0.62).
narrative_ontology:measurement_basis(cree_be_t1054, observed).
narrative_ontology:measurement(cree_be_t1204, creed_381_pneumatology__filioque_reading, base_extractiveness, 1204, 0.72).
narrative_ontology:measurement_basis(cree_be_t1204, observed).
narrative_ontology:measurement(cree_be_t1439, creed_381_pneumatology__filioque_reading, base_extractiveness, 1439, 0.78).
narrative_ontology:measurement_basis(cree_be_t1439, observed).
narrative_ontology:measurement(cree_be_t1870, creed_381_pneumatology__filioque_reading, base_extractiveness, 1870, 0.82).
narrative_ontology:measurement_basis(cree_be_t1870, observed).
narrative_ontology:measurement(cree_be_t2020, creed_381_pneumatology__filioque_reading, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement_basis(cree_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t589, creed_381_pneumatology__filioque_reading, suppression_requirement, 589, 0.2).
narrative_ontology:measurement_basis(cree_su_t589, observed).
narrative_ontology:measurement(cree_su_t800, creed_381_pneumatology__filioque_reading, suppression_requirement, 800, 0.4).
narrative_ontology:measurement_basis(cree_su_t800, observed).
narrative_ontology:measurement(cree_su_t1054, creed_381_pneumatology__filioque_reading, suppression_requirement, 1054, 0.6).
narrative_ontology:measurement_basis(cree_su_t1054, observed).
narrative_ontology:measurement(cree_su_t1204, creed_381_pneumatology__filioque_reading, suppression_requirement, 1204, 0.75).
narrative_ontology:measurement_basis(cree_su_t1204, observed).
narrative_ontology:measurement(cree_su_t1439, creed_381_pneumatology__filioque_reading, suppression_requirement, 1439, 0.7).
narrative_ontology:measurement_basis(cree_su_t1439, observed).
narrative_ontology:measurement(cree_su_t1870, creed_381_pneumatology__filioque_reading, suppression_requirement, 1870, 0.8).
narrative_ontology:measurement_basis(cree_su_t1870, observed).
narrative_ontology:measurement(cree_su_t2020, creed_381_pneumatology__filioque_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement_basis(cree_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, identity_coordination).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__ecumenical_reunion_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, papal_primacy_infallibility_1870).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Filioque controversy' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints — three readings of one kernel (creed_381_pneumatology). This story (filioque_reading) authors the clause-as-enforced-communion-condition under magisterial clarification, with epsilon priced for the standing arrangement's polity (who decides, who pays). monoprocession_reading authors the same received text under an inviolability-without-consent regime, with the extraction structure inverted. ecumenical_reunion_reading authors a bilateral-recognition regime in which both formulations persist regionally and enforcement demobilizes. Each carries its own epsilon, beneficiaries, and victims; the family is linked through affects_constraints. The upstream/downstream gradient runs from the doctrinal-content dispute (upstream, most established) to the authority claim (downstream, where the extraction lives), since the content claim is cited as warrant for the authority claim; this story additionally feeds forward into the 1870 crystallization of papal primacy and infallibility, whose clarification-authority premise this arrangement first established.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(creed_381_pneumatology__filioque_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
