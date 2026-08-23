% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Catastrophe-Memory Mourning Practice — Symbol Continuity Reading
 *   domain: religious/collective_memory
 *
 * SUMMARY:
 *   A catastrophe-marked diaspora community maintains a fixed
 *   mourning-practice regime: an annual calendar of fasts and lamentations,
 *   an unaltered liturgy, and a standing prohibition on modifying the
 *   inherited forms. This file instantiates the SYMBOL-CONTINUITY READING of
 *   the catastrophe_memory_kernel — the claim that the regime exists to
 *   preserve symbolic continuity and collective identity across generations.
 *   Per Rule 1, the contest among the four readings is not described or
 *   adjudicated inside this constraint; the sibling readings are separate
 *   constraint files, and the committer structure is routed to the omega
 *   variables. The epsilon referent is the standing mourning-practice
 *   arrangement itself, assessed by this reading's own lights: the
 *   arrangement genuinely produces the identity-continuity it exists to
 *   produce, while imposing real but bounded rigidity costs on members whose
 *   lives or proposals do not fit the inherited forms. The claim/metric gap
 *   is deliberate and independent: claimed_type is authored from structural
 *   analysis (coordination function plus asymmetric rigidity cost plus active
 *   enforcement), while the metrics are authored as descriptive best
 *   estimates — the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - rabbinic_custodial_authority: agenda-setter (institutional / identity_locked) — administers the calendar, certifies the liturgy, rules on alterations; trustee of the unbroken chain
 *   - ritual_officiants: beneficiary (organized / constrained) — cantors, readers, lay leaders whose standing and skill-capital attach to the inherited forms
 *   - diaspora_congregation_members: primary beneficiary with payer secondary (moderate / constrained) — receive the identity-continuity the arrangement produces; absorb fit-costs where the fixed forms collide with their lives
 *   - liturgical_reform_advocates: payer (moderate / constrained) — propose abbreviation, translation, addition; bear procedural delay, custodial veto, and social cost
 *   - younger_generation_members: payer with beneficiary secondary (powerless / constrained) — inherit forms whose original context they never knew; no standing in liturgical governance
 *   - marginalized_households: payer (powerless / trapped) — mixed marriages, shift workers, isolated families; the fixed rite excludes their circumstances and exit would sever ties they cannot replace
 *   - secular_memory_activists: excluded (organized / mobile) — commemorate the same catastrophe outside the religious frame; no seat in the liturgical conversation
 *   - ritual_studies_scholars: observer (analytical / analytical) — comparative study of liturgy and collective memory; no stake in persistence or alteration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.33).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.38).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.33).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Catastrophe-Memory Mourning Practice — Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'f68f553c-e45e-4a62-8477-9e72f169a2cb').
narrative_ontology:cs_kernel_codification('f68f553c-e45e-4a62-8477-9e72f169a2cb', fixed_text).
narrative_ontology:cs_authority_grounding('f68f553c-e45e-4a62-8477-9e72f169a2cb', lineage).
narrative_ontology:cs_interpretation_layer_present('f68f553c-e45e-4a62-8477-9e72f169a2cb').
narrative_ontology:cs_reading_relation('f68f553c-e45e-4a62-8477-9e72f169a2cb', catastrophe_memory_kernel__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('f68f553c-e45e-4a62-8477-9e72f169a2cb', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('f68f553c-e45e-4a62-8477-9e72f169a2cb', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('f68f553c-e45e-4a62-8477-9e72f169a2cb', foundational, symbolic_continuity_is_the_primary_yield).
narrative_ontology:cs_axiom_status(symbolic_continuity_is_the_primary_yield, holdable).
narrative_ontology:cs_axiom_grounding('f68f553c-e45e-4a62-8477-9e72f169a2cb', symbolic_continuity_is_the_primary_yield, instrumental).
narrative_ontology:cs_axiom('f68f553c-e45e-4a62-8477-9e72f169a2cb', foundational, unbroken_form_is_constitutive_of_meaning).
narrative_ontology:cs_axiom_status(unbroken_form_is_constitutive_of_meaning, holdable).
narrative_ontology:cs_axiom_grounding('f68f553c-e45e-4a62-8477-9e72f169a2cb', unbroken_form_is_constitutive_of_meaning, conventional).
narrative_ontology:cs_reference_frame('f68f553c-e45e-4a62-8477-9e72f169a2cb', unbroken_symbolic_transmission_chain).
narrative_ontology:cs_drift_state('f68f553c-e45e-4a62-8477-9e72f169a2cb', fourth_generation_present, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f68f553c-e45e-4a62-8477-9e72f169a2cb', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, rabbinic_custodial_authority).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, ritual_officiants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, diaspora_congregation_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, liturgical_reform_advocates).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, marginalized_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, diaspora_congregation_members).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, tradition_continuity_doctrine).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, unbroken_transmission_precedent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Learned councils and senior clergy who administer the mourning calendar, certify the liturgy, and rule on proposed alterations. They inherited custody from the catastrophe generation and understand themselves as trustees of an unbroken chain owed to the dead and to future generations. Altering the forms is within their formal power but is experienced as breach of trust; they defend core fixity case by case while permitting additive commemorations. Leaving the role would mean abandoning the office and the self-concept built on custody.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, rabbinic_custodial_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Cantors, readers, and lay leaders who perform the fixed rites. Their training, standing, and often livelihood attach to the inherited forms; mastery of the unchanging text is their professional capital. They collect honor and role-security from the arrangement without setting it. Retraining for altered forms would be costly but not impossible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_officiants, beneficiary,
    organized, biographical, constrained, regional).

% Ordinary households across the diaspora who attend the annual mourning observances, fund commemorations, and teach the forms to their children. They receive the identity-continuity the arrangement exists to produce and name it among the community's core goods. The same households bear fit-costs where the fixed calendar or liturgy collides with work schedules, family composition, or local circumstance — costs most absorb quietly as the price of belonging. Drifting away is possible but carries real social and familial cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, diaspora_congregation_members, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, diaspora_congregation_members, payer).

% Educated members — often teachers, writers, or younger professionals — who propose abbreviations, translations, or new commemorations. Proposals meet procedural delay, custodial veto, and social cost: advocates acquire reputations as restless or disloyal. They remain inside the community by choice; their objection is precisely that they want the community to keep them, so exit would concede the point they are arguing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, liturgical_reform_advocates, payer,
    moderate, biographical, constrained, continental).

% Children and grandchildren of members who inherit forms whose original context they never knew. Participation is often rote; the language and references can feel distant. They hold no formal standing in liturgical governance. Their realistic options are rote participation, quiet disengagement, or eventual exit — and their aggregate drift is the main demographic pressure the custodial authority watches.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_members, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__symbol_continuity_reading, younger_generation_members, beneficiary).

% Households whose circumstances the fixed rite does not accommodate — mixed marriages, shift workers, geographically isolated families, members with disabilities the liturgy assumes away. They want belonging inside the tradition but encounter exclusion or accommodation-by-exception at each fixed observance. Leaving would sever family and communal ties they cannot replace; staying means repeated exclusion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, marginalized_households, payer,
    powerless, biographical, trapped, regional).

% Descendants and cultural organizers who commemorate the catastrophe through museums, archives, arts, and civic ceremonies outside the religious frame. They carry the same memory with different instruments and have no seat in the liturgical conversation; several began as reform advocates who concluded the religious channel was closed to them and built parallel institutions instead.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, secular_memory_activists, excluded,
    organized, generational, mobile, national).

% Academic observers of liturgy and collective memory who study the practice comparatively. They publish analyses of how the forms evolved and what they carry, take no side in communal disputes, and hold no stake in the arrangement's persistence or alteration.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, ritual_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__symbol_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, stateless community's remembrance onto a shared symbolic calendar: fixed dates, fixed lament-liturgy, and inherited forms synchronize memory across generations and geographies, solving the collective-action problem of transmitting identity once the living witnesses are gone.
% TRANSFER_FUNCTION: Moves attendance, fasting labor, recitation, and commemoration funding from ordinary members into the maintenance of the fixed forms; moves standing and adjudicative authority toward the custodial class; moves no material wealth out of the paying seats — the extracted quantity is adaptability, paid by those whose lives or proposals do not fit the inherited forms.
% ABSENT_VOICES: Secular descendant organizations and civic memorialists carry the same catastrophe memory outside the religious frame and have no seat in the liturgical conversation; intermarried spouses attend without standing to propose alteration; the youngest members are spoken for by their elders. Their absence lets the fixity present itself as unanimous when it is merely unchallenged in the room.
% DISAPPEARANCE_RATIONALE: If the fixed mourning-practice vanished overnight, the community loses its symbolic clock: households would remember privately, civic bodies would absorb public commemoration, and within a generation or two the dispersed population would lack a common identity scaffold. Congregations, officiant roles, and the custodial authority itself would dissolve or reorganize around thinner substitutes — the arrangements of every named seat depend on the regime's persistence.
% FOUNDING_PROBLEM: In the generation after the catastrophe, survivors facing dispersal and fading living memory needed a repeatable, teachable form that would bind remembrance into communal identity before the last witnesses died.
% FOUNDING_PROBLEM_CORROBORATION: Memory-studies historians and demographers outside the benefiting parties corroborate the founding problem's reality — living memory measurably decays across roughly eighty years, and transmission requires deliberate structure — while documenting that civic, archival, and familial channels now partially perform the same function, supporting the reformers' claim that the rite's monopoly on the solution has ended. Secular descendant associations attest continued identity without the fixed rite. No source outside the beneficiary set attests that the problem remains unsolved; only the custodial authority itself makes that claim.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.33, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.33 at interval end) because the arrangement transfers no material wealth out of the paying seats — the extracted quantity is adaptability, and the gains (continuity, belonging, meaning) are received broadly by the same population that pays. Suppression (0.38) is a raw structural property, unscaled by power or scope: enforcement today is habitual and social rather than juridical, but the enforcement machinery exists and is required in principle — without some sanction, form-deviation would compound. Theater ratio (0.24) reflects real rote performance in the observances alongside genuine symbolic function; it is far from the piton range. Accessibility collapse is low (0.40): alternatives demonstrably persist — civic commemoration, museum and archive practice, private family memory, reform communities — and understanding the arrangement does not close them off. Resistance (0.45) is recurring rather than constant: periodic reform movements, generational pushback, and secular exit. The temporal series run on one shared grid (t = 0, 20, 40, 60, 80, 100, 120) with every tracked metric authored at every point. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: sanction intensity builds as the witness generation dies (peak 0.70 at t=40, when conformity must be enforced rather than remembered) and then decays across the second century as habituation and voluntary compliance take over — an enforcement-decay arc, not a static picture. The theater_ratio series oscillates (0.30 at t=60, dip at t=80, rise, dip at t=120): the driver is anniversary economics and periodic renewal movements (centenary commemorations, digital-archive engagement), a side effect of memory dynamics rather than an intermittent-reinforcement extraction mechanism. Identity-coordination cover-story check: the identity function here is the reading's own subject matter, not a wrapper; the fixity_load_bearing_question omega tests whether the identity-work is real, and the Power x Scope coupling is inspected — extraction concentrates mildly on powerless seats at wide scope, which is flagged for review rather than excused by the identity_coordination complexity offset. The type-default Boltzmann floor is used; no override is warranted.
 *
 * PERSPECTIVAL GAP:
 *   The custodian seat and the payer seats should compute differently, and the structural data supports that divergence. From the rabbinic_custodial_authority position, the arrangement is a sacred trust it administers at real personal cost — its exit is identity_locked, so it experiences the fixity it enforces as obligation, not imposition. From the liturgical_reform_advocates and marginalized_households positions, the same structure operates as a rigidity tax levied on exactly the members least able to pay it, enforced by procedural veto and social sanction. The diaspora_congregation_members seat sits between: net beneficiaries who nonetheless quietly absorb fit-costs. Ritual_officiants experience the arrangement as professional capital. Because exit options differ sharply across seats inside the same community — identity_locked for custodians, constrained for members and reformers, trapped for marginalized households, mobile for those already outside — the engine should compute materially different per-seat classifications from identical global metrics. The authored claim does not adjudicate this divergence; it is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to real structural relationships. The custodial authority sits near the beneficiary end (administers, sustains authority through custody, bears little of the rigidity cost). Ritual_officiants collect standing without running the arrangement. Diaspora_congregation_members are declared beneficiaries with a payer secondary role: they receive the continuity the arrangement exists to produce and bear diffuse fit-costs, placing them mildly beneficiary-side of symmetric. Liturgical_reform_advocates are declared victims: they pay in sanction and stalled proposals. Younger_generation_members and marginalized_households are declared victims, but both remain inside the benefit circle — one is a dual-positioned inheritor, the other an attached-but-excluded household — so a directionality override is authored for the powerless power atom at d = 0.60: derivation from victim-listing alone would place both near full-target (~0.85), overstating extraction for agents who continue to receive the arrangement's core good. Residual imprecision is acknowledged: marginalized_households plausibly sit nearer 0.70 than 0.60, and the override cannot differentiate within the atom. On the receipt surface: gain_flow is authored as 'diffuse' as an affirmative claim after checking every named seat — the custodial authority accrues authority and the officiants accrue standing, but those are benefits-from the arrangement's persistence, not receipts of the extracted quantity; the extracted adaptability is simply foregone, received by no seat. fixing_cost is authored 'prohibitive': for the seat that could fix it, loosening the forms risks the continuity function the majority values plus schism, vastly exceeding the relief gained by the paying minority — with the caveat, recorded here, that the authority does permit additive commemorations at the margin while protecting core fixity. The engine owns the arithmetic on all of this; the diffuse-plus-prohibitive cell may read piton-side, and the low theater ratio, live contested founding problem, and centenary engagement upturn are the data that bear against that reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding remembrance into repeatable communal form before living memory faded — is authored as contested: custodial sources attest the forgetting-danger is permanent and the problem live; reform advocates and secular descendant organizations attest the acute founding conditions passed and that civic and familial channels now partially perform the same function. Corroboration from outside the benefiting parties (memory-studies historians, demographers, secular descendant associations) supports both halves, which is why the status is contested rather than dead. With status=contested and disappearance_verdict=world_rearranges, no mismatch flag fires and mandatrophy is not resolved: the arrangement's mandate has not outlived its function. The classification discipline prevents two opposite mislabels. Calling this a snare would erase the genuine coordination function — a dispersed, stateless population really does solve its identity-transmission problem through this arrangement, and the gains are broadly received. Calling it a rope would erase the real asymmetry — identifiable seats (reform advocates, the young, marginalized households) pay rigidity costs through the same structure that coordinates everyone else, under active enforcement. Tangled_rope holds both facts. The piton label is guarded against by the evidence of live function: theater ratio well below the degradation range, oscillating rather than monotonically rising, and engagement upturns at anniversary renewals — although the diffuse-plus-prohibitive receipt cell is honestly recorded and left for the engine to weigh.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the catastrophe_memory_kernel (symbol_continuity_reading). What would each sibling reading change structurally if adopted as the operative account?',
    'Cross-reading comparison across the four linked constraint files: contrast epsilon values, victim sets, and failure modes. The survival_competence_reading would re-price rigidity costs against lost adaptive capacity; the trauma_encoding_reading would price re-traumatization; the boundary_maintenance_reading would price excluded outsiders.',
    'If the survival reading is correct, this reading''s low extractiveness is miscalibrated — the fixity cost includes forfeited survival yield, raising effective extraction. If this reading is correct, the siblings over-price the rite by charging it with functions it does not perform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one of four readings of the catastrophe-memory kernel; sibling adoption changes the victim set and epsilon referent weighting.').

omega_variable(
    fixity_load_bearing_question,
    'Is the unbroken-form requirement still doing identity-work, or has it become inertia maintained by custodial interest and habit?',
    'Natural experiment from communities that adapted the forms (reform schisms, translated liturgies, added vernacular commemorations): if identity-continuity outcomes hold where forms were adapted, the fixity norm is partially vestigial; if continuity measurably degrades, the fixity is load-bearing.',
    'If vestigial, the constraint drifts toward the piton profile (theatrical maintenance of an atrophied requirement) and the rigidity costs become pure deadweight; if load-bearing, the coordination function justifies the enforcement the tangled_rope claim asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fixity_load_bearing_question, empirical, 'Whether form-fixity is functional for symbolic continuity or inertial.').

omega_variable(
    internalized_fixity_norm,
    'Is member compliance with the fixed forms driven by external communal sanction or by internalized identity (members defending the fixity they personally experience as costly)?',
    'Post-exit trajectory of leavers: if former members shed fixity-commitment quickly after leaving, the norm was externally enforced; if they carry it into new communities or transmit it to children outside the enforcing structure, it is internalized.',
    'If internalized, the scalar suppression measure understates the constraint''s hold — the enforcement travels inside members and persists where the sanctioning apparatus is absent; the effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_fixity_norm, empirical, 'Structural versus internalized enforcement of the fixity norm.').

omega_variable(
    abstract_beneficiary_resolution,
    'Does the continuity benefit resolve to identifiable actor seats (custodians, officiants, members), or does it genuinely accrue to no actor — a pure trans-generational public good?',
    'Rent-distribution analysis: enumerate what each named seat would concretely lose if the rite were frozen, adapted, or abolished, and whether any seat''s loss is disproportionate to its contribution. Watch specifically for custodial-authority rents that scale with fixity rather than with continuity.',
    'If a seat''s stake scales with fixity itself rather than with continuity outcomes, a capturer exists, gain_flow ''diffuse'' fails, and the constraint drifts toward the snare side of the hybrid range. If losses are proportionate and universal, the diffuse receipt stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abstract_beneficiary_resolution, empirical, 'Whether the tradition-continuity beneficiary resolves to a capturing seat or remains a genuine public good.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_symbol_continuity_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(catmem_symbol_continuity_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(catmem_symbol_continuity_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(catmem_symbol_continuity_tr_t60, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement(catmem_symbol_continuity_tr_t80, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(catmem_symbol_continuity_tr_t100, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 100, 0.29).
narrative_ontology:measurement(catmem_symbol_continuity_tr_t120, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 120, 0.24).

% Extraction over time
narrative_ontology:measurement(catmem_symbol_continuity_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(catmem_symbol_continuity_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(catmem_symbol_continuity_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(catmem_symbol_continuity_be_t60, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 60, 0.31).
narrative_ontology:measurement(catmem_symbol_continuity_be_t80, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(catmem_symbol_continuity_be_t100, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 100, 0.32).
narrative_ontology:measurement(catmem_symbol_continuity_be_t120, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 120, 0.33).

% Suppression requirement over time
narrative_ontology:measurement(catmem_symbol_continuity_su_t0, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(catmem_symbol_continuity_su_t20, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(catmem_symbol_continuity_su_t40, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(catmem_symbol_continuity_su_t60, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(catmem_symbol_continuity_su_t80, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement(catmem_symbol_continuity_su_t100, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 100, 0.44).
narrative_ontology:measurement(catmem_symbol_continuity_su_t120, catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 120, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'catastrophe-memory ritual' decomposes into four structurally distinct claims about what the mourning-practice regime is and does, per the epsilon-invariance principle. This file instantiates the symbol-continuity reading (epsilon 0.33: symbolic transmission with bounded rigidity costs). The survival-competence sibling claims operational-yield transmission and prices rigidity against lost adaptive capacity; the trauma-encoding sibling claims warning-system function and prices re-traumatization; the boundary-maintenance sibling claims exclusionary function and prices excluded outsiders. The symbol-continuity reading is the most established descriptive account and functions as the carrier medium the others cite; its fixity commitment structurally bears on the survival reading, and its yield-claim outright contradicts the survival reading's foundational axiom. Each reading is a separate constraint with its own epsilon, beneficiaries, and victims; none averages over the others. All four files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__symbol_continuity_reading, powerless, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
