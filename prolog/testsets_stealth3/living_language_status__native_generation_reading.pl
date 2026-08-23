% ============================================================================
% CONSTRAINT STORY: living_language_status__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__native_generation_reading, []).

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
 *   constraint_id: living_language_status__native_generation_reading
 *   human_readable: Native-Generational Transmission Standard for Language Vitality
 *   domain: sociolinguistic/political
 *
 * SUMMARY:
 *   A definitional standard circulates through ministries of education,
 *   language academies, international vitality frameworks, and nationalist
 *   rhetoric: a language counts as living only where children acquire it
 *   natively at home and carry it into daily life; communities that sustain a
 *   tongue solely through recitation, memorization, and ritual study are told
 *   they are preserving a corpse. The standard has a real analytic core —
 *   intergenerational transmission is among the strongest predictors of
 *   language survival — and performs real coordinating work: agencies triage
 *   revitalization money, compare cases across countries, and set curriculum
 *   priorities by it. Deployed as the sole arbiter of vitality, it also sorts
 *   winners and losers: movements that can plausibly nativize gain a
 *   legitimacy instrument, while liturgical-only communities absorb a
 *   standing status cost and lose eligibility ground. KEY AGENTS (by
 *   structural relationship): secular_nationalist_movements: primary
 *   beneficiary (organized/identity_locked) — converts the standard into
 *   linguistic-sovereignty legitimacy; their origin narratives depend on it.
 *   state_language_bureaus: agenda setter (institutional/constrained) —
 *   codifies the standard into curricula, academies, and funding gates;
 *   collects mandate and budget.
 *   endangered_language_revitalization_movements: secondary beneficiary
 *   (organized/constrained) — voluntary adopters using the standard for
 *   urgency and triage. liturgical_only_communities: primary payer
 *   (organized/identity_locked) — absorbs the corpse-preservation framing,
 *   standing loss, and program exclusion. adult_second_language_speakers:
 *   secondary payer with partial benefit (moderate/mobile) — permanent
 *   second-tier certification beneath nativeness.
 *   traditionalist_religious_leadership: excluded voice (organized/trapped) —
 *   objected to the codified standard from outside the codifying venues.
 *   academic_sociolinguists: analytical observer with agenda-setting leverage
 *   — supplies the empirical warrant and audits applications. This file
 *   instantiates ONE reading of the living_language_status kernel; the
 *   sibling readings are separate constraints with their own epsilon and
 *   loser sets (see commentary.kernel_context). Claim and metrics are
 *   authored independently: the reading is claimed as tangled_rope because
 *   its operation pairs a genuine coordination function with asymmetric,
 *   enforcement-dependent costs; the metrics describe that operation without
 *   being tuned toward the claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__native_generation_reading, 0.58).
domain_priors:suppression_score(living_language_status__native_generation_reading, 0.42).
domain_priors:theater_ratio(living_language_status__native_generation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(living_language_status__native_generation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__native_generation_reading, "Native-Generational Transmission Standard for Language Vitality").
narrative_ontology:topic_domain(living_language_status__native_generation_reading, "sociolinguistic/political").

domain_priors:requires_active_enforcement(living_language_status__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__native_generation_reading, 'a1e9b752-20be-4098-b21c-b4c7383d3743').
narrative_ontology:cs_kernel_codification('a1e9b752-20be-4098-b21c-b4c7383d3743', distributed).
narrative_ontology:cs_authority_grounding('a1e9b752-20be-4098-b21c-b4c7383d3743', distributed).
narrative_ontology:cs_reading_relation('a1e9b752-20be-4098-b21c-b4c7383d3743', living_language_status__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('a1e9b752-20be-4098-b21c-b4c7383d3743', living_language_status__literary_continuity_reading, forecloses).
narrative_ontology:cs_axiom('a1e9b752-20be-4098-b21c-b4c7383d3743', foundational, generational_transmission_is_the_life_criterion).
narrative_ontology:cs_axiom_status(generational_transmission_is_the_life_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a1e9b752-20be-4098-b21c-b4c7383d3743', generational_transmission_is_the_life_criterion, empirically_contingent).
narrative_ontology:cs_axiom('a1e9b752-20be-4098-b21c-b4c7383d3743', foundational, liturgical_recitation_preserves_a_corpus_not_a_tongue).
narrative_ontology:cs_axiom_status(liturgical_recitation_preserves_a_corpus_not_a_tongue, holdable).
narrative_ontology:cs_axiom_grounding('a1e9b752-20be-4098-b21c-b4c7383d3743', liturgical_recitation_preserves_a_corpus_not_a_tongue, empirically_contingent).
narrative_ontology:cs_axiom('a1e9b752-20be-4098-b21c-b4c7383d3743', secondary, native_speakers_arbitrate_authenticity).
narrative_ontology:cs_axiom_status(native_speakers_arbitrate_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('a1e9b752-20be-4098-b21c-b4c7383d3743', native_speakers_arbitrate_authenticity, conventional).
narrative_ontology:cs_reference_frame('a1e9b752-20be-4098-b21c-b4c7383d3743', native_household_transmission_norm).
narrative_ontology:cs_drift_state('a1e9b752-20be-4098-b21c-b4c7383d3743', contemporary_comparativist_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a1e9b752-20be-4098-b21c-b4c7383d3743', '2026-08-05T09:30:00Z').
narrative_ontology:cs_kernel_id(living_language_status__native_generation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, endangered_language_revitalization_movements).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, liturgical_only_communities).
narrative_ontology:constraint_victim(living_language_status__native_generation_reading, adult_second_language_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__native_generation_reading, adult_second_language_speakers).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, intergenerational_transmission_survival_thesis).
narrative_ontology:constraint_vindicates(living_language_status__native_generation_reading, linguistic_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Champion nation-building programs in which the nation's language must be heard in streets and homes to certify national continuity. The native-transmission standard hands them a decisive argument: wherever their people speak the ancestral tongue as a mother tongue, the nation is demonstrably alive; wherever it survives only in ritual, rivals can dismiss the nation as custodian of a relic. Their origin stories — exile, return, linguistic rebirth — are welded to the standard, and abandoning it would unravel the legitimacy narrative they run on. What flows to them is legitimacy and mobilizing power, not administered programs.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, secular_nationalist_movements, beneficiary,
    organized, generational, identity_locked, national).

% Ministries of education, national language academies, and cultural agencies adopt the transmission standard into official vitality assessments, school curricula, grammar and spelling authorities, and grant eligibility rules. They decide which languages count as living, which receive teacher training, broadcasting quotas, and orthographic investment, and which are logged as objects of documentation rather than support. Operating the standard brings budget lines, staffing, and statutory authority; revising or dropping it would invalidate curricula and funding formulas their institutions are built around.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, state_language_bureaus, agenda_setter,
    institutional, generational, constrained, national).

% Community movements for threatened tongues — immersion schools, home-transmission campaigns, master-apprentice programs — rely on the transmission standard to argue urgency and steer scarce funding toward producing child speakers. The standard gives their case empirical teeth and a metric comparable across countries; it also narrows what counts as success, pressing them to prioritize household transmission over other goals their communities hold.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, endangered_language_revitalization_movements, beneficiary,
    organized, generational, constrained, regional).

% Communities whose ancestral language survives in scripture recitation, prayer, memorized chant, and scholarly commentary rather than kitchen-table speech — diaspora congregations, monastic orders, recitation lineages. Under the standard, everything they maintain registers as preservation of a dead thing: their fluency is framed as ritual performance, their continuity record dismissed as embalming. They bear the cost in public standing, in eligibility for language-support programs, and in younger members' willingness to invest in a skill officially labeled lifeless. Leaving the practice would mean leaving the tradition itself, so they stay and absorb the framing — some internalizing it as embarrassment about their own heritage.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, liturgical_only_communities, payer,
    organized, civilizational, identity_locked, global).

% Adults who learn an ancestral or revived language through schooling, immersion courses, or community classes reach real communicative competence yet never register as native transmitters. Under the standard their achievement permanently reads as second-tier: they staff the public sphere the standard celebrates while being told the language's life resides elsewhere, in households raising native children. Many simultaneously gain employment, civic participation, and belonging through the very proficiency the standard ranks below nativeness.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, adult_second_language_speakers, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, adult_second_language_speakers, beneficiary).

% Rabbis, priests, cantors, and lay traditionalist scholars who contested the standard as it was codified — insisting ritual continuity is its own form of life and that revival-by-policy distorts the sacred tongue. They stood largely outside the secular academies, ministries, and expert committees where the definition hardened; their objections arrived after the fact as dissent to be managed rather than as co-authorship of the measure.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, traditionalist_religious_leadership, excluded,
    organized, civilizational, trapped, global).

% Researchers who operationalize language vitality — graded scales, transmission indices, international factor frameworks — supply the empirical warrant the standard runs on and audit its applications. Their findings that transmission strongly predicts survival anchor the criterion; their critiques of cases where native transmission coexists with shrinking domains, and of complementarity between ritual literacy and later nativization, strain its exclusivity. They collect neither the gains nor the losses; they hold definitional authority in reserve.
narrative_ontology:constraint_stakeholder(living_language_status__native_generation_reading, academic_sociolinguists, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__native_generation_reading, academic_sociolinguists, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__native_generation_reading, secular_nationalist_movements).
narrative_ontology:fixing_cost_class(living_language_status__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, operationalizable criterion for assessing language vitality — enabling triage of revitalization funding, cross-country comparability, documentation-priority ranking, and curriculum investment choices, on a variable (child acquisition at home) that is empirically tractable where 'aliveness' otherwise is not.
% TRANSFER_FUNCTION: Moves legitimacy and resources from communities sustaining a language through recitation and study (delegitimated as keepers of a dead thing) toward nativization programs, immersion infrastructures, and the nationalist narratives that sponsor them; moves definitional authority to whoever controls transmission metrics.
% ABSENT_VOICES: Traditionalist religious leadership and the liturgical communities' own scholars were largely absent from the venues where the standard was codified — secular academies, ministries of education, international expert panels. Their objection, that ritual continuity is itself a form of life, entered mostly as post-hoc dissent rather than co-authorship; they pair with the excluded stakeholder seat.
% DISAPPEARANCE_RATIONALE: If the standard vanished overnight, vitality assessment would lose its dominant yardstick: revitalization funding triage, international vitality classifications, academy mandates, and nationalist legitimacy narratives would all need refounding on a different criterion or none — redistributing support away from transmission-centric programs and rehabilitating the public standing of liturgical-only communities.
% FOUNDING_PROBLEM: Late nineteenth-century nation-builders needed to demonstrate that a people dispossessed of territory and statehood nonetheless possessed a living national essence; a criterion under which the nation's language counted as alive only where the nation spoke it natively converted demographic presence into proof of continuity — and separated 'true heirs' who raise children in the tongue from mere custodians of texts.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the beneficiary set: sociolinguistic vitality research and international expert frameworks adopted transmission metrics on analytic grounds independent of nationalist sponsorship, confirming the founding problem's continuing salience; historians documenting liturgical communities' reactions corroborate the delegitimating turn from the paying side. Revivalist institutions also attest the problem as live — precisely the self-serving attestation the genealogy interview treats as cover-story risk, which is why the external seats carry the weight here.
narrative_ontology:disappearance_verdict(living_language_status__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__native_generation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.58: the standard's costs are real but bounded — liturgical communities retain internal standing, sibling readings survive in parallel institutions, and part of the standard's operation is plain assessment rather than rent-taking. Suppression at 0.42 reflects gatekeeping rather than force: eligibility rules, curricula, and editorial conventions close doors, nobody is coerced over prayer language, and the closing is partial because religious and literary spheres keep rival definitions alive. Theater_ratio 0.28: anniversary celebrations of revived tongues, vitality rankings, and heritage pageantry increasingly perform conviction the underlying practice questions, while most activity remains functional assessment. Accessibility_collapse 0.55: once the standard anchors an assessment regime, rival definitions do not vanish — they are demoted to sentiment inside official venues while surviving outside, so alternatives half-collapse. Resistance 0.60: traditionalist leadership, liturgical communities, and parts of the discipline have contested the framing continuously since codification; it has never gone unchallenged. Suppression is authored as a raw structural property and stays unscaled; only extractiveness gets scaled by directionality and scope inside the engine. The measurement series share one time grid — every tracked metric is authored at every examined point (0 through 120 in steps of 20) — so temporal reads sample complete rows. Trajectories: institutionalization drives extraction and enforcement buildup to a plateau near t=80–100, with late easing as complementarity evidence accumulates and sibling readings regain institutional ground. Receipt surface: the characteristic gain — legitimacy certified by native speech — accrues to the nationalist movements (bureaus collect administrative mandate, but the delegitimation dividend lands on the movements' narrative), hence gain_flow names that seat; fixing the standard would demand coordinated revision of academy statutes, curricula, and funding formulas across many polities while the benefits land mostly on others, so fixing_cost is prohibitive for whoever could act.
 *
 * PERSPECTIVAL GAP:
 *   From the bureau seat the standard is infrastructure it administers — a working tool that earns its keep in triage and comparability. From the nationalist seat it is a legitimacy engine: the difference between a nation and a museum. From the liturgical seat the same artifact is a verdict pronounced on an entire continuity — a judgment its bearers did not author and cannot appeal, delivered in venues they were excluded from. The adult-learner seat experiences a quieter cut: full participation in the language's public life paired with permanent second-class certification. Same-power divergence matters here: liturgical_only_communities and endangered_language_revitalization_movements hold similar organized capacity, yet one sits at the bearing end and the other at the subsidized end — the differentiator is metric fit and exit structure, not raw power. The engine computes these divergent per-seat readings from the power, exit, and role data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations drive the derivation. Secular_nationalist_movements and endangered_language_revitalization_movements sit in the beneficiary set with identity-locked or constrained exit — subsidy-side directionality, strongest for the movements whose core narrative the standard certifies, since identity fusion removes any hedging strategy. State_language_bureaus are agenda-setters deriving partial benefit from mandate and budget, placing them low-mid. Liturgical_only_communities are the declared payer population with identity-locked exit — near the full-target end, because they cannot arbitrage the framing: the standing cost lands at full weight. Adult_second_language_speakers are declared payers with mobile exit and a genuine secondary benefit — target-leaning but materially damped, since they can and do reclassify their own achievement. Identity-lock mechanisms differ by seat: for the nationalist movements it is narrative identity (the rebirth story constitutes the movement), for liturgical communities relational-practical identity (the practice constitutes the tradition; exiting the language means exiting the covenant). No directionality overrides were authored: the beneficiary/victim declarations plus exit options already separate the seats the way the situations describe.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification holds two misreadings apart. Read as pure extraction, the standard's genuine analytic function disappears — along with the reason revitalization movements adopt it voluntarily and the reason international frameworks codified it on analytic grounds. Read as pure coordination, the standing status cost borne by liturgical communities evaporates into 'assessment overhead,' and the nationalist sponsorship that keeps the exclusivity version enforced goes unexamined. Tangled_rope keeps both halves visible: coordination (triage, comparability, urgency signaling) and asymmetric cost-bearing (delegitimation converting into legitimacy) ride the same definitional structure, held in place by active institutional enforcement against sibling definitions. Mandatrophy is not resolved — the founding problem, certifying collective vitality through native speech, is still actively pursued, so no sunset or retirement applies. The drift to watch is the opposite of classic mandatrophy: not a dead function kept alive theatrically, but a live function slowly acquiring a ceremonial layer (anniversary rhetoric, ranking rituals) while its exclusivity claim loses analytic ground — visible in the theater_ratio series if it accelerates past roughly 0.5.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vitality_definition_kernel_omega,
    'This story instantiates only the native_generation_reading of the living_language_status kernel; which reading governs a given assessment changes the entire beneficiary and bearing structure — where exactly does the disagreement sit?',
    'Locate the disputed element — the definiens of ''life'' for a language: household transmission (this file), textual productivity (literary_continuity_reading), or ritual continuity (liturgical_preservation_reading). Corpus comparison across the three sibling stories shows extraction relocating wholesale with whichever reading governs.',
    'Under literary_continuity the presses and periodicals become the vital core and nativist gatekeeping becomes the bearing structure; under liturgical_preservation the roles invert entirely and nativist campaigns become the cost-imposers. Cross-reading conclusions are only valid as separate-file comparisons joined by network edges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vitality_definition_kernel_omega, conceptual, 'One-of-three kernel readings; sibling files instantiate the other two definiens.').

omega_variable(
    liturgy_as_precondition_omega,
    'Is liturgical preservation a rival to native transmission, as the reading''s polemic asserts, or its historical precondition?',
    'Comparative history of language revivals: whether populations with deep ritual-literacy layers (centuries of memorized text, commentarial schooling) subsequently nativize at higher rates than populations without such layers, controlling for institutional investment.',
    'If precondition, the reading''s core polemic (''preservation of a corpse'') is self-undermining — liturgical communities become unrecognized suppliers of the very nativity the standard certifies, and the cost assessment must credit them as net contributors; if rival, the current structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgy_as_precondition_omega, empirical, 'Complementarity versus rivalry between ritual transmission and native transmission.').

omega_variable(
    status_cost_internalization_omega,
    'Is the standing cost borne by liturgical-only communities chiefly structural (program eligibility, funding gates, official classification) or internalized (younger members absorbing the dead-language framing and disinvesting from the skill)?',
    'Cohort tracing after gates open or official framings soften: if disinvestment in liturgical fluency persists in communities once eligibility barriers drop, a large share of the cost is carried internally rather than imposed externally.',
    'If internalized, the standard''s damage outruns its enforcement machinery — softening official criteria will not restore standing, and remedies must address inherited self-assessment rather than program design alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_cost_internalization_omega, empirical, 'Structural versus internalized mechanism of the standing cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__native_generation_reading, 0, 130).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lls_nat_gen_tr_t0, living_language_status__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lls_nat_gen_tr_t20, living_language_status__native_generation_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(lls_nat_gen_tr_t40, living_language_status__native_generation_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(lls_nat_gen_tr_t60, living_language_status__native_generation_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(lls_nat_gen_tr_t80, living_language_status__native_generation_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(lls_nat_gen_tr_t100, living_language_status__native_generation_reading, theater_ratio, 100, 0.27).
narrative_ontology:measurement(lls_nat_gen_tr_t120, living_language_status__native_generation_reading, theater_ratio, 120, 0.28).

% Extraction over time
narrative_ontology:measurement(lls_nat_gen_be_t0, living_language_status__native_generation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lls_nat_gen_be_t20, living_language_status__native_generation_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(lls_nat_gen_be_t40, living_language_status__native_generation_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(lls_nat_gen_be_t60, living_language_status__native_generation_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(lls_nat_gen_be_t80, living_language_status__native_generation_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(lls_nat_gen_be_t100, living_language_status__native_generation_reading, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(lls_nat_gen_be_t120, living_language_status__native_generation_reading, base_extractiveness, 120, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lls_nat_gen_su_t0, living_language_status__native_generation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(lls_nat_gen_su_t20, living_language_status__native_generation_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(lls_nat_gen_su_t40, living_language_status__native_generation_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement(lls_nat_gen_su_t60, living_language_status__native_generation_reading, suppression_requirement, 60, 0.41).
narrative_ontology:measurement(lls_nat_gen_su_t80, living_language_status__native_generation_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(lls_nat_gen_su_t100, living_language_status__native_generation_reading, suppression_requirement, 100, 0.46).
narrative_ontology:measurement(lls_nat_gen_su_t120, living_language_status__native_generation_reading, suppression_requirement, 120, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__native_generation_reading, information_standard).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__native_generation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'living language' decomposes into three structurally distinct claims with different epsilon and different bearing sets: native generational transmission (this file), continuous ritual use, and productive literary work. They form one constraint family; every member links the others. The native-generation reading is upstream of the liturgical reading polemically — its corpse verdict defines what the liturgical reading must defend against — and downstream of the literary reading historically: the literate substrate produced by textual-revival movements supplied much of what later nativization campaigns drew on. Each file carries its own epsilon, stakeholders, and classification; nothing here averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
