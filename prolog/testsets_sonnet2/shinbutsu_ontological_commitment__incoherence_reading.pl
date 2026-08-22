% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__incoherence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__incoherence_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__incoherence_reading
 *   human_readable: Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   From roughly the ninth century until the 1868 shinbutsu bunri
 *   (kami-buddha separation) edicts, kami and buddhas were ritually,
 *   institutionally, and administratively co-present in Japan without any
 *   single stabilized account of what their relationship actually was.
 *   Honji-suijaku theory offered one metaphysical account (kami as local
 *   manifestations of buddhas), but it never achieved doctrinal monopoly;
 *   countless shrine-temple complexes operated with locally improvised,
 *   mutually inconsistent, and often simply unstated understandings of how
 *   the two systems related. This reading treats that absence of settlement
 *   as the load-bearing structural fact: shinbutsu-shugo was not a synthesis
 *   anyone believed but an institutionally tolerated incoherence that let
 *   shrine-temple administrative complexes, ritual specialists, and (much
 *   later) Meiji state-builders each use the ambiguity for their own purposes
 *   without ever being forced to resolve it. The syncretic_reading
 *   (honji-suijaku as a genuine unified cosmology) and partition_reading
 *   (clean domain separation between life-cycle Shinto and afterlife
 *   Buddhism) are separate constraints describing the SAME kernel
 *   differently; this file is the incoherence reading only, per the
 *   ε-invariance decomposition rule.
 *
 * KEY AGENTS:
 *   - shrine_temple_administrative_complexes: agenda_setter (institutional/arbitrage) — administer joint shrine-temple sites, collect revenue and standing from both kami and buddha cults without needing to reconcile them
 *   - shomin_lay_practitioners_seeking_coherent_cosmology: payer (powerless/constrained) — participate in rituals whose ultimate metaphysical stakes (what happens after death, which deity actually answers prayer) are never authoritatively settled for them
 *   - shugendo_and_syncretic_ritual_specialists: payer/beneficiary (moderate/constrained) — build careers on ambiguity-navigating ritual expertise, precarious once any settlement (either direction) removes the need for their mediating function
 *   - kokugaku_and_later_shinto_nativist_scholars: beneficiary (organized/mobile) — retrospectively diagnose the incoherence as confusion or degeneration to be corrected, gaining intellectual and later political authority from the diagnosis
 *   - meiji_state_builders: beneficiary (institutional/arbitrage) — use the diagnosed incoherence as the justification for forcibly separating kami and buddha institutions in 1868, consolidating State Shinto
 *   - kami_priests_absorbed_into_temple_hierarchies: payer (powerless/trapped) — occupied subordinate ritual positions within Buddhist-dominated shrine-temple complexes for centuries under the tolerated ambiguity, then displaced again at separation
 *   - buddhist_temple_hierarchy: observer/beneficiary (institutional/constrained) — held administrative and often doctrinal seniority over many joint complexes prior to 1868; loses standing when the incoherence reading is later weaponized for separation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, 0.58).
domain_priors:suppression_score(shinbutsu_ontological_commitment__incoherence_reading, 0.42).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__incoherence_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__incoherence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__incoherence_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__incoherence_reading, "Shinbutsu-shugo as Institutionally Tolerated Ontological Incoherence").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__incoherence_reading, "religious_studies/japanese_history/ontology_of_practice").

domain_priors:requires_active_enforcement(shinbutsu_ontological_commitment__incoherence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__incoherence_reading, '1c561dbb-7142-4ba6-87f8-28aaadacf052').
narrative_ontology:cs_kernel_codification('1c561dbb-7142-4ba6-87f8-28aaadacf052', distributed).
narrative_ontology:cs_authority_grounding('1c561dbb-7142-4ba6-87f8-28aaadacf052', distributed).
narrative_ontology:cs_reading_relation('1c561dbb-7142-4ba6-87f8-28aaadacf052', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('1c561dbb-7142-4ba6-87f8-28aaadacf052', shinbutsu_ontological_commitment__partition_reading, influences).
narrative_ontology:cs_axiom('1c561dbb-7142-4ba6-87f8-28aaadacf052', foundational, ontological_unsettlement_is_the_operative_fact).
narrative_ontology:cs_axiom_status(ontological_unsettlement_is_the_operative_fact, holdable).
narrative_ontology:cs_axiom_grounding('1c561dbb-7142-4ba6-87f8-28aaadacf052', ontological_unsettlement_is_the_operative_fact, empirically_contingent).
narrative_ontology:cs_axiom('1c561dbb-7142-4ba6-87f8-28aaadacf052', secondary, institutional_actors_profit_from_nonresolution).
narrative_ontology:cs_axiom_status(institutional_actors_profit_from_nonresolution, holdable).
narrative_ontology:cs_axiom_grounding('1c561dbb-7142-4ba6-87f8-28aaadacf052', institutional_actors_profit_from_nonresolution, empirically_contingent).
narrative_ontology:cs_reference_frame('1c561dbb-7142-4ba6-87f8-28aaadacf052', pre_settlement_dual_tradition_coexistence).
narrative_ontology:cs_drift_state('1c561dbb-7142-4ba6-87f8-28aaadacf052', meiji_separation_edicts_1868, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('1c561dbb-7142-4ba6-87f8-28aaadacf052', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrative_complexes).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, kokugaku_and_later_shinto_nativist_scholars).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, shomin_lay_practitioners_seeking_coherent_cosmology).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, shugendo_and_syncretic_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, kami_priests_absorbed_into_temple_hierarchies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__incoherence_reading, shugendo_and_syncretic_ritual_specialists).
narrative_ontology:constraint_victim(shinbutsu_ontological_commitment__incoherence_reading, buddhist_temple_hierarchy).
narrative_ontology:constraint_vindicates(shinbutsu_ontological_commitment__incoherence_reading, religious_practice_can_outrun_doctrinal_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer joint kami-buddha sites (jingu-ji), setting ritual calendars, collecting land revenue and tax exemptions under overlapping claims, and adjudicating local precedence disputes without ever needing higher doctrinal authority to settle what kami and buddhas actually are to each other. The ambiguity is administratively convenient: settling it in either direction would force a jurisdictional reckoning they have no incentive to invite.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrative_complexes, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, shrine_temple_administrative_complexes, beneficiary).

% Participate in shrine and temple rites for harvest, healing, and death without a stable account of which deity's efficacy actually operates or what happens to them cosmologically after death. Cannot exit the ambiguity — there is no alternative coherent local religious institution to patronize instead — and bear the diffuse cost of unresolved metaphysical stakes at moments (mortality, crisis) when clarity would matter most to them.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shomin_lay_practitioners_seeking_coherent_cosmology, payer,
    powerless, biographical, constrained, local).

% Build ritual careers and mountain-ascetic institutions on the specific expertise of navigating the unsettled kami-buddha terrain for lay clients — a genuine coordination service. Their livelihood depends on the ambiguity persisting; any authoritative settlement (in either direction) would eliminate the mediating function they are paid for, so they are exposed payers whenever settlement pressure rises even though they currently benefit from the status quo.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, shugendo_and_syncretic_ritual_specialists, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, shugendo_and_syncretic_ritual_specialists, beneficiary).

% Occupy subordinate ritual positions within Buddhist-dominated joint complexes for centuries, their kami-associated functions treated as secondary within an institutional hierarchy that never had to formally justify the subordination because the underlying relationship was never doctrinally fixed. They cannot appeal to a settled cosmology to assert equal standing, and are displaced again — not restored — when the incoherence is later resolved by force at Meiji separation.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, kami_priests_absorbed_into_temple_hierarchies, payer,
    powerless, generational, trapped, local).

% Diagnose the centuries-long absence of ontological settlement as evidence of Buddhist corruption of a purer, prior Shinto tradition. Their intellectual and political capital grows directly from characterizing shinbutsu-shugo as incoherence to be corrected rather than synthesis to be honored — this reading of the kernel is the resource their scholarly and later political careers are built on.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, kokugaku_and_later_shinto_nativist_scholars, beneficiary,
    organized, generational, mobile, national).

% Inherit the diagnosed incoherence as the historiographical justification for the 1868 shinbutsu bunri edicts, forcibly separating shrine and temple institutions and consolidating a state-controlled Shinto establishment. They do not create the incoherence but they are the most concentrated late beneficiary of the incoherence_reading specifically — it licenses a state-building project that would be far harder to justify against either the syncretic or partition readings.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, meiji_state_builders, beneficiary,
    institutional, civilizational, arbitrage, national).

% Held administrative and often doctrinal seniority over many joint shrine-temple complexes for centuries under the tolerated ambiguity, benefiting from the arrangement much as the administrative complexes did, but becomes a concentrated payer at the moment the incoherence diagnosis is weaponized for Meiji separation, losing land, institutional standing, and control over previously joint sites.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__incoherence_reading, buddhist_temple_hierarchy, observer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(shinbutsu_ontological_commitment__incoherence_reading, buddhist_temple_hierarchy, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_commitment__incoherence_reading, diffuse).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_commitment__incoherence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows two independently rooted religious traditions occupying the same sacred geography to coexist administratively without either being forced to concede primacy or exit — a real coordination problem in a landscape where site, calendar, and audience overlapped extensively.
% TRANSFER_FUNCTION: Moves administrative flexibility and revenue-jurisdiction stability toward shrine-temple complexes and (centuries later) political justification toward Meiji state-builders, while moving cosmological uncertainty and subordinate ritual status onto lay practitioners and kami priests absorbed into temple hierarchies.
% ABSENT_VOICES: Individual lay practitioners with acute cosmological need (the dying, the bereaved, those seeking a specific deity's intervention) have no institutional voice in whether the kami-buddha relationship gets settled; their felt need for coherence is not represented in either the medieval administrative arrangements or the Meiji-era resolution, both of which were negotiated among institutional actors, not on behalf of lay metaphysical anxiety.
% DISAPPEARANCE_RATIONALE: If the tolerated incoherence had been forced to resolve at any point before 1868 — either into a settled honji-suijaku metaphysics or a clean domain partition — shrine-temple jurisdictional arrangements, revenue splits, and ritual precedence across hundreds of joint complexes would have had to be renegotiated, kami priests' subordinate status would have had grounds for challenge or would have been formally entrenched, and the Meiji state would have lacked the specific 'confused syncretism' narrative it used to justify forced separation — it would have needed a different justificatory resource for State Shinto, or the project might have taken a substantially different form.
% FOUNDING_PROBLEM: How to administer overlapping sacred sites and audiences claimed by two independently rooted religious traditions (kami cults tied to local and imperial lineage, Buddhism arriving from the continent) without a costly turf war over precedence, land, and patronage.
% FOUNDING_PROBLEM_CORROBORATION: Edo-period shogunate temple-shrine registration records (terauke and jingu administrative documentation) show settled, custom-based jurisdictional arrangements by the seventeenth century, corroborating from outside both the shrine-temple administrators and the later nativist scholars that the original turf-war problem was substantially resolved through custom long before the incoherence was diagnosed as a live crisis; independent historians of Japanese religion (e.g., studies of jingu-ji institutional records) attest the same discontinuity between settled administrative practice and the later politically motivated incoherence narrative.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__incoherence_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__incoherence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__incoherence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__incoherence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__incoherence_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_ontological_commitment__incoherence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_ontological_commitment__incoherence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (rising from 0.31) because the arrangement's costs are initially diffuse (lay practitioners simply live without cosmological settlement, a mild cost) but escalate as different institutional actors increasingly weaponize the unsettled kernel for their own advantage — first shrine-temple complexes exploiting jurisdictional ambiguity for revenue and standing, then nativist scholars and Meiji state-builders exploiting the SAME diagnosed incoherence to justify forced separation that primarily benefited the state and displaced kami priests and ritual specialists who had built careers on the ambiguity. Theater ratio is authored high (0.61 at end) because a great deal of the visible ritual and doctrinal apparatus (honji-suijaku exegesis, joint liturgical calendars, shared precinct arrangements) increasingly functioned as performed coherence covering an administrative reality that never required the performance to be true. Suppression is moderate (0.42) and rises across the interval — not suppression of alternative metaphysics (many circulated freely) but suppression of the DEMAND for settlement: institutional actors had strong incentives to prevent anyone from forcing the question, since forcing it would have required choosing winners among overlapping revenue and status claims. Accessibility collapse is authored low-moderate (0.35) because alternative, more coherent framings (partition, syncretic) were always available in the discourse and never fully closed off — this supports tangled_rope rather than snare, since suppression of settlement is real but not total. Resistance is moderate (0.4): lay practitioners occasionally pressed for clarity (deathbed anxieties, sectarian disputes) but rarely had standing to force resolution against administratively entrenched complexes.
 *
 * PERSPECTIVAL GAP:
 *   From the shrine-temple administrative seat, shinbutsu-shugo looks like reasonable institutional flexibility — a rope: why force a costly, divisive doctrinal settlement when ambiguity lets everyone keep their existing claims? From the lay practitioner or displaced kami-priest seat, the same arrangement looks like tolerated incoherence maintained BECAUSE resolving it would cost the institutions above them something — closer to tangled_rope or even snare, depending on how much agency one attributes to the administrators versus how much the incoherence was simply inherited rather than actively cultivated. The engine's per-seat computation should reflect this: administrators compute near-rope, targets compute near-extractive, and the aggregate tangled_rope classification captures that both a genuine (if minimal) coordination function and asymmetric extraction coexist in the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Shrine-temple administrative complexes are the clearest structural beneficiaries: their institutional survival depended on NOT resolving the ontological question, since either resolution (pure kami primacy or pure buddha primacy) would have stripped one half of their revenue and land-claim base. Meiji state-builders are a temporally displaced beneficiary — they inherit the SAME unsettled kernel centuries later and extract political value from diagnosing it as incoherent, using that diagnosis to justify a forced separation that consolidated state authority over Shinto. Lay practitioners and kami priests absorbed into Buddhist-dominated hierarchies are targets: they bore the lived cost of unresolved cosmology (uncertain ritual efficacy, subordinate status) without capturing any of the administrative flexibility the ambiguity purchased for institutions above them. Shugendo and syncretic ritual specialists occupy a dual position — their expertise is PRODUCED by the ambiguity (a genuine, if narrow, coordination function: someone has to navigate the unsettled terrain for lay clients) but they are also exposed as payers whenever settlement pressure rises, since clarity of either kind threatens their mediating role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how do you administer a religious landscape where two independent cult traditions (imperial-court-linked kami worship and continental Buddhism) arrived at overlapping sacred sites without either displacing the other — was genuinely live for centuries: early syncretic arrangements solved a real coordination problem (which deity gets precedence at which site, on which calendar day, funded how). But by the Edo period, with shrine-temple jurisdictions, tax exemptions, and ritual precedence long settled through custom and shogunate registration, the ORIGINAL problem was substantially dead — the ambiguity persisted not because a live coordination problem still required it, but because unsettling it would have reopened settled property and status arrangements. This is exactly the mandatrophy pattern: a genuinely coordinative origin (avoid a costly two-tradition turf war) persisting long after the turf was settled, maintained because someone benefits from NOT reopening the question — and then, ironically, that very persistence becomes the pretext Meiji reformers use to justify a different extraction (forced separation for state consolidation) dressed as a return to purity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incoherence_vs_undertheorized_synthesis,
    'Was shinbutsu-shugo genuinely ontologically incoherent (no stable commitment ever existed) or was it a coherent but informally theorized synthesis that later observers mistake for incoherence because it was never systematized into a single doctrinal text?',
    'Close reading of medieval ritual manuals (goma liturgies, honji-suijaku commentarial texts) for internal consistency across sites and sects; if practice varied by site with no shared underlying logic, incoherence is supported; if a shared implicit logic recurs across independent sites, the syncretic reading gains ground.',
    'If the arrangement was a genuine synthesis rather than tolerated incoherence, this story''s beneficiary structure (administrators profiting from ambiguity) dissolves and the constraint reclassifies toward rope or mountain (a real, if implicit, cosmological achievement) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_undertheorized_synthesis, conceptual, 'Whether the absence of a single stable ontological commitment reflects genuine incoherence or an undertheorized but functioning synthesis.').

omega_variable(
    meiji_retrospective_construction,
    'Is the ''incoherence'' reading itself partly a retrospective construction by Meiji-era nativist scholars and state Shinto architects who needed shinbutsu-shugo to look separable and confused in order to justify the 1868 shinbutsu bunri separation edicts?',
    'Compare pre-Meiji internal Buddhist and Shinto commentarial self-descriptions (do practitioners themselves describe the arrangement as unstable or ambiguous?) against post-1868 nativist historiography describing it as confused or degenerate.',
    'If pre-Meiji sources show practitioners regarding the arrangement as stable and meaningful, the incoherence reading is substantially a Meiji-era beneficiary construction, strengthening this story''s tangled_rope classification (coordination cover for a later extraction/separation agenda) rather than a naive description of medieval religious life.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_retrospective_construction, empirical, 'Whether the incoherence framing predates or postdates the Meiji separation project that benefited from it.').

omega_variable(
    kernel_framing_underdetermination,
    'Two coherent framings of the underlying kernel (shinbutsu ontological commitment) exist: the obvious framing treats shinbutsu-shugo as a doctrinal-content question (what did practitioners believe kami and buddhas were); the less obvious framing treats it as an institutional-authority question (who had standing to declare what kami and buddhas were, and whether that standing was ever exercised). Under the doctrinal-content framing this reading (incoherence) is well-supported by the absence of a canonical synthesis text; under the institutional-authority framing, the arrangement may have been perfectly stable AS AN ADMINISTRATIVE MATTER (shrine-temple complexes had settled jurisdictional arrangements) even while doctrinally unresolved.',
    'Examine whether jurisdictional/administrative stability (land rights, ritual precedence, tax status of shrine-temple complexes) correlates with doctrinal resolution or is orthogonal to it across multiple documented complexes (e.g., Kumano, Hie, Usa Hachiman).',
    'If administrative stability is high while doctrinal resolution is low, the classification should route toward this reading''s tangled_rope (administratively functional, doctrinally untethered, cover for jurisdictional extraction) rather than toward a pure mountain (natural absence of ontology) or pure rope (harmless flexible coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Doctrinal-content framing vs institutional-authority framing of the ontological commitment kernel produce different support for the incoherence claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__incoherence_reading, 0, 700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 100, 0.44).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 300, 0.5).
narrative_ontology:measurement(shin_tr_t500, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 500, 0.55).
narrative_ontology:measurement(shin_tr_t650, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 650, 0.58).
narrative_ontology:measurement(shin_tr_t700, shinbutsu_ontological_commitment__incoherence_reading, theater_ratio, 700, 0.61).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 0, 0.31).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 300, 0.44).
narrative_ontology:measurement(shin_be_t500, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 500, 0.5).
narrative_ontology:measurement(shin_be_t650, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 650, 0.55).
narrative_ontology:measurement(shin_be_t700, shinbutsu_ontological_commitment__incoherence_reading, base_extractiveness, 700, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 300, 0.28).
narrative_ontology:measurement(shin_su_t500, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 500, 0.34).
narrative_ontology:measurement(shin_su_t650, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 650, 0.39).
narrative_ontology:measurement(shin_su_t700, shinbutsu_ontological_commitment__incoherence_reading, suppression_requirement, 700, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__incoherence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(shinbutsu_ontological_commitment__incoherence_reading, 0.1).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, shinbutsu_bunri_meiji_separation_edicts).
narrative_ontology:affects_constraint(shinbutsu_ontological_commitment__incoherence_reading, state_shinto_establishment).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the shinbutsu_ontological_commitment kernel (incoherence_reading, syncretic_reading, partition_reading), each authored as a separate constraint with its own ε per the ε-invariance decomposition rule. incoherence_reading is authored with the highest ε (0.58) because it locates the extractable resource in the UNSETTLEDNESS itself, benefiting administrative complexes and later Meiji state-builders. It also structurally influences the downstream Meiji-era separation and State Shinto constraints, since the incoherence diagnosis is the historiographical resource those later arrangements draw on to justify forced separation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(shinbutsu_ontological_commitment__incoherence_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
