% ============================================================================
% CONSTRAINT STORY: classical_latin_standard__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_classical_latin_standard__continuity_reading, []).

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
 *   constraint_id: classical_latin_standard__continuity_reading
 *   human_readable: Living-Transmission Standard of Correct Latin (Continuity Reading)
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the continuity reading of the
 *   classical_latin_standard kernel as a clean, single-epsilon constraint:
 *   correct Latin is the living form carried by unbroken practice, and
 *   natural drift within that transmission is legitimate development. The
 *   standard's arbiter is transmitted usage itself, administered in practice
 *   by the institutions that carry the transmission — ecclesiastical bodies,
 *   grammar schools, universities. The arrangement solves a real coordination
 *   problem (a trans-regional, trans-temporal learned medium for a fragmented
 *   Europe) while charging a real gatekeeping price: access to correct usage
 *   runs through institutional formation, and the same arbiter seats that
 *   absorb drift as development mark deviant usage as barbarism. The victim
 *   set is deliberately minimal — the reading does not systematically
 *   delegitimize practice outside the standard; it excludes barbarisms and
 *   charges admission. KEY AGENTS (by structural relationship):
 *   ecclesiastical_institutions — agenda-setter and chief beneficiary
 *   (institutional/mobile, global); grammar_school_and_university_faculties —
 *   arbiter-beneficiary (institutional/identity_locked, continental);
 *   learned_correspondence_networks — beneficiary (organized/constrained,
 *   continental); self_taught_latin_writers — minimal victim
 *   (powerless/constrained, regional); regional_variety_latinists — minimal
 *   victim (moderate/trapped, regional); seminarians_and_grammar_pupils —
 *   admission-cost bearers (powerless/constrained, regional);
 *   vernacular_advocates — excluded (organized/arbitrage, continental);
 *   philological_historians — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(classical_latin_standard__continuity_reading, 0.38).
domain_priors:suppression_score(classical_latin_standard__continuity_reading, 0.15).
domain_priors:theater_ratio(classical_latin_standard__continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(classical_latin_standard__continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(classical_latin_standard__continuity_reading, tangled_rope).
narrative_ontology:human_readable(classical_latin_standard__continuity_reading, "Living-Transmission Standard of Correct Latin (Continuity Reading)").
narrative_ontology:topic_domain(classical_latin_standard__continuity_reading, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(classical_latin_standard__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(classical_latin_standard__continuity_reading, '05473412-7a1e-4d28-b245-7598bee12371').
narrative_ontology:cs_kernel_codification('05473412-7a1e-4d28-b245-7598bee12371', implicit).
narrative_ontology:cs_authority_grounding('05473412-7a1e-4d28-b245-7598bee12371', practice).
narrative_ontology:cs_interpretation_layer_present('05473412-7a1e-4d28-b245-7598bee12371').
narrative_ontology:cs_reading_relation('05473412-7a1e-4d28-b245-7598bee12371', classical_latin_standard__reconstruction_reading, forecloses).
narrative_ontology:cs_reading_relation('05473412-7a1e-4d28-b245-7598bee12371', classical_latin_standard__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('05473412-7a1e-4d28-b245-7598bee12371', foundational, transmitted_usage_constitutes_correctness).
narrative_ontology:cs_axiom_status(transmitted_usage_constitutes_correctness, holdable).
narrative_ontology:cs_axiom_grounding('05473412-7a1e-4d28-b245-7598bee12371', transmitted_usage_constitutes_correctness, conventional).
narrative_ontology:cs_axiom('05473412-7a1e-4d28-b245-7598bee12371', foundational, unbroken_transmission_sustains_authority).
narrative_ontology:cs_axiom_status(unbroken_transmission_sustains_authority, holdable).
narrative_ontology:cs_axiom_grounding('05473412-7a1e-4d28-b245-7598bee12371', unbroken_transmission_sustains_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('05473412-7a1e-4d28-b245-7598bee12371', unbroken_living_transmission).
narrative_ontology:cs_drift_state('05473412-7a1e-4d28-b245-7598bee12371', contemporary_philological_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05473412-7a1e-4d28-b245-7598bee12371', '').
narrative_ontology:cs_kernel_id(classical_latin_standard__continuity_reading, classical_latin_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, grammar_school_and_university_faculties).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, learned_correspondence_networks).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, self_taught_latin_writers).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, regional_variety_latinists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(classical_latin_standard__continuity_reading, seminarians_and_grammar_pupils).
narrative_ontology:constraint_victim(classical_latin_standard__continuity_reading, seminarians_and_grammar_pupils).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, living_usage_arbiter_doctrine).
narrative_ontology:constraint_vindicates(classical_latin_standard__continuity_reading, unbroken_transmission_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the schools, seminaries, chanceries, and liturgy through which the transmitted form is carried; corrects usage and thereby defines what counts as barbarism; collects deference, staffing control, and the transnational identity that a shared learned language makes possible. Could in principle shift to vernaculars (and partly has), but while it maintains the tradition it is the standard's chief carrier and collects its principal non-monetary returns.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, ecclesiastical_institutions, beneficiary).

% Teaches the transmitted forms, examines candidates, and corrects deviations; collects formation fees and the arbiter authority that constitutes teaching careers. The office and the tradition are the same thing — a master who stopped teaching the transmitted standard would dissolve his own position, so exit is professional and institutional self-annihilation rather than a relocation.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, grammar_school_and_university_faculties, beneficiary,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, grammar_school_and_university_faculties, agenda_setter).

% Scholars, officials, and letter-writers who read and write the shared language across borders and generations; they receive the trans-temporal medium and contribute the usage the standard absorbs as development. Leaving means losing the learned republic's audience; alternatives (Greek, vernaculars) exist but at the cost of rebuilding a readership.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, learned_correspondence_networks, beneficiary,
    organized, biographical, constrained, continental).

% Acquires Latin outside institutional formation and submits usage to correction by arbiters it did not choose; forms that deviate from transmitted practice are marked barbarous regardless of communicative effectiveness, and access to Latin-mediated standing runs through institutional certification that must be purchased with years of formation.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, self_taught_latin_writers, payer,
    powerless, biographical, constrained, regional).

% Communities whose living regional usage of the learned language drifts from the transmitted core. The reading legitimizes drift in principle, but when local usage drifts beyond the tolerated band it is marked corrupt, and the community must re-learn standard forms or lose standing. The variety is not detachable from the community that speaks and writes it, so abandoning it means abandoning the community's own idiom.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, regional_variety_latinists, payer,
    moderate, biographical, trapped, regional).

% Bears the admission cost directly — years of drills, composition exercises, and correction under institutional arbiters — and receives, at the end of formation, the competence for which the standard alone confers standing. The cost is paid now and the return collects over a career, so the seat is dual-positioned with a near-term cost bias.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, seminarians_and_grammar_pupils, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(classical_latin_standard__continuity_reading, seminarians_and_grammar_pupils, beneficiary).

% Writers and scholars who argued that learned work could and should be carried in vernaculars. They stood outside the chanceries and faculties where correctness and the standard's scope were decided, and they built the alternative medium that eventually displaced the standard's functional territory. Their exclusion from the arbiter conversation is the reason the standard's scope was set without them.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, vernacular_advocates, excluded,
    organized, generational, arbitrage, continental).

% Studies the transmission record directly — manuscript strata, the Romance divergence, the Carolingian reform, the humanist correction — and can see the full structure: what the transmitted chain actually looks like, where it was renewed from texts, and how the rival accounts of correctness divide the same evidence.
narrative_ontology:constraint_stakeholder(classical_latin_standard__continuity_reading, philological_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(classical_latin_standard__continuity_reading, grammar_school_and_university_faculties).
narrative_ontology:fixing_cost_class(classical_latin_standard__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single written learned language usable across politically fragmented regions and across centuries — theology, law, diplomacy, and scholarship conducted in one medium when no vernacular had continental reach — while giving the learned community a stable-but-developing standard that absorbs new usage as it emerges instead of freezing it.
% TRANSFER_FUNCTION: Moves certification of correctness and access to Latin-mediated standing (clerical office, university degree, chancery post, scholarly publication) from unformed aspirants to the institutions that charge formation time, fees, and conformity; moves arbiter authority over usage to the transmitted tradition's institutional carriers.
% ABSENT_VOICES: The writers whose usage constitutes the drift the standard absorbs — ordinary practitioners and regional communities — hold no arbiter seat: their usage becomes 'development' when the institutions adopt it and 'barbarism' when they do not, and they are not present where that line is drawn. Vernacular advocates were likewise outside the chanceries and faculties that decided the standard's scope.
% DISAPPEARANCE_RATIONALE: If the continuity standard vanished overnight, the remaining Latin-using domains would reorganize within a generation: ecclesiastical use would shift fully to vernaculars or English, Neo-Latin composition would lose its arbiter and fragment, and the arrangement's rival accounts of correctness would lose their referent, since each is defined against living transmission. The learned world already rearranged once, slowly, when vernaculars displaced the standard's functional scope; removing the standard removes the remaining structure the same way.
% FOUNDING_PROBLEM: After the Western Empire's administrative collapse, Latin remained the only medium of literacy, law, and worship spanning the successor kingdoms while its native-speaker base dissolved into the Romance vernaculars; the founding problem was keeping a single workable learned standard alive across a politically fragmenting, linguistically diverging Europe.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the transformation of the Roman world corroborate the founding problem's reality from outside the benefiting parties: post-Roman Europe demonstrably needed a supraregional learned medium and Latin demonstrably served it. On current status, sociolinguistic and Neo-Latin scholarship outside the tradition attests the functional displacement by vernaculars, while the tradition's own offices attest the problem is still live (a supranational church and a trans-temporal scholarly corpus still need a common medium) — which is exactly why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(classical_latin_standard__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(classical_latin_standard__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(classical_latin_standard__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(classical_latin_standard__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(classical_latin_standard__continuity_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(classical_latin_standard__continuity_reading_tests).
:- end_tests(classical_latin_standard__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end, peaking 0.46 when Latin was the sole credential for law, medicine, theology, and diplomacy): the gatekeeping is real — admission to correct usage runs through paid institutional formation and subjection to arbiter correction — but there is no systematic delegitimization of outside practice, and the standard absorbs the very drift it does not control. Suppression is low (0.15): the reading legitimizes drift, corrects rather than destroys, and its enforcement machinery (school statutes, correction, examination) built up through the scholastic period, peaked at the humanist conflict, then decayed as vernaculars took over the functions — which is why suppression_requirement is authored on the shared grid despite the static low endpoint. Theater rises steadily (0.10 to 0.35) as vernaculars displaced Latin's working functions and a growing share of remaining use became ceremonial (mottos, diplomas, liturgical formulae) — a Goodhart signature at the domain level, though within the remaining domains (ecclesiastical use, Neo-Latin scholarship) the standard still performs real work. Accessibility_collapse 0.35: alternatives (vernacular learned media, reconstructed Classical, hybrid forms) remain workable throughout. Resistance 0.40: the humanist attack on medieval Latinity, vernacular movements, and reconstructionist philology met the standard repeatedly without displacing its institutional core until vernacular displacement worked from outside. All three series run on one shared grid (t = 0, 20, 40, 60, 80, 100; t=0 approximates the Carolingian consolidation around 800 CE, t=100 the present), so no metric is sampled against another metric's end-state value at earlier times. For the receipt surface: the gatekeeping rent demonstrably lands on the faculties (formation fees, examiner authority, career control), with ecclesiastical institutions collecting a parallel share in deference and staffing; and fixing is prohibitive for the only seats that could fix it — the carriers — since removal costs the tradition's accumulated precision and their own function, far exceeding the moderate harm the gatekeeping does.
 *
 * PERSPECTIVAL GAP:
 *   From inside the arbiter seats the standard is not experienced as a constraint at all — it is what correct Latin IS; correcting barbarism feels like maintaining the language itself, and charging formation feels like the only way the competence can exist. From the payer seats the same structure is a gate they must purchase: their usage is judged by arbiters they did not choose, under criteria (transmitted usage) their own formation did not give them access to except at institutional prices. The engine should compute divergent per-seat classifications from this asymmetry: beneficiaries holding arbiter authority experience coordination; payers with constrained or trapped exit experience extraction through the same structure. The excluded vernacular advocates experienced a third thing — not extraction but exclusion from the conversation in which the standard's scope was decided — and their arbitrage exit (building the vernacular alternative) is what eventually shrank the standard's territory.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical institutions and the faculties sit near the beneficiary end: the standard subsidizes their identity, staffing, and authority, and their exit is costly in the extreme (identity-locked for the faculties, whose office is the tradition). Learned correspondents benefit from the medium and pay formation-and-conformity costs — near-symmetric with a beneficiary lean. The minimal victim seats — self-taught writers and regional-variety users — sit near the target end: they bear correction and exclusion without collecting arbiter authority, and their exit is constrained or trapped (a regional variety is not detachable from the community that speaks it). Seminarians and pupils are dual-positioned: they pay the admission cost now and collect the competence later, placing them near symmetric with a slight target lean on a biographical horizon. The reading's structural signature is that the SAME transmitted-usage standard both coordinates the learned community and concentrates arbiter rent in the institutions that certify usage — the tangled-rope shape, which is why the claim is tangled_rope rather than rope: participants are not all net beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a snare would erase the genuine coordination function — the trans-temporal learned medium was real and the victim set is genuinely minimal; classifying it as a rope would erase the gatekeeping rent — arbiter authority and formation fees flow through the same structure that coordinates. Tangled_rope holds both facts. The mandatrophy question is live: the founding problem (a learned medium for a Europe whose vernaculars were diverging) has largely been solved by the vernaculars themselves, yet the arrangement persists in ecclesiastical and scholarly niches — the rising theater ratio tracks exactly this migration of function toward performance. The R5 mismatch consumer should read founding_problem_status=contested against disappearance_verdict=world_rearranges: the world would rearrange (the remaining domains would reorganize around English and national vernaculars, and the rival accounts of correctness would lose their referent), but the original mandate is no longer the whole of what holds the arrangement in place. Piton classification is avoided honestly: within its remaining domains the arbiter function still collects and still delivers, so the theater is a shrinking-domain symptom rather than the whole structure — though if the functional domains empty further while the ceremonial share keeps rising, this story should be re-authored as drifting toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unbroken_transmission_discontinuity,
    'Is the chain of transmitted practice actually unbroken, or was the correct form twice restarted from texts — at the Carolingian reform, when living usage had diverged into the Romance vernaculars, and again in the humanist Ciceronian correction?',
    'Philological stratification of 7th-10th century usage: whether pre-Carolingian living usage flows continuously into the reformed school standard or whether the reform constitutes a discontinuous textual restart; the same test applied to the 15th-century humanist correction of the standard.',
    'If the chain is broken, the reading''s empirically contingent foundational axiom fails; the reading collapses toward the reconstruction or hybrid sibling, its beneficiaries lose the legitimacy claim that grounds their arbiter authority, and the gatekeeping extraction that authority authorizes loses its warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbroken_transmission_discontinuity, empirical, 'Whether the transmission chain is genuinely unbroken or was textually restarted at documented reforms.').

omega_variable(
    drift_legitimacy_boundary,
    'Where does legitimate development end and barbarism begin, and is that boundary drawn by structural criteria (comprehensibility, continuity of usage) or by the institutional preference of the arbiter seats?',
    'Comparative analysis of developments the tradition absorbed (post-Classical vocabulary, ecclesiastical coinages, syntactic simplifications) against forms it marked barbarous, testing whether acceptance tracks breadth of usage or the institutional origin of the form.',
    'If the boundary is institutional preference, the reading''s low-suppression profile is partial cover and effective extraction rises above the authored value; if the boundary is structural, the reading''s coordination claim is strengthened and the moderate extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drift_legitimacy_boundary, conceptual, 'Principled versus institutional boundary between legitimate drift and barbarism.').

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is the continuity reading of the classical_latin_standard kernel; what structural facts would the sibling readings change, and where exactly is the disagreement located?',
    'Adopting the reconstruction reading would invert the beneficiary/victim structure: post-Classical practice becomes the victim set, textual philologists become the arbiters, and suppression of drift becomes high. Adopting the hybrid reading would split the arbiter seat between textual fidelity and development-recognition, producing an intermediate victim set and intermediate epsilon. The disagreement is located in the arbiter assignment — living usage versus recoverable Classical form — and in the moral status of drift (development versus corruption).',
    'Classification, beneficiary/victim sets, and suppression values all shift with the reading; the three stories are separate constraints with separate epsilon, not one constraint with a measurement parameter, and cross-reading comparison must run through the network edges rather than by re-measuring this story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: what each sibling reading of the same kernel would structurally change.').

omega_variable(
    formation_cost_necessity,
    'Is the institutional formation the standard requires a genuine cost of transmitting competence, or a rent — could correct usage be formed and certified outside the arbiter institutions at comparable quality?',
    'Compare competence outcomes and total costs across formation paths: institutional seminaries and schools versus self-directed study with modern corpora, corrected texts, and immersion communities outside the traditional institutions.',
    'If formation cost systematically exceeds the competence it delivers, the gatekeeping component is rent and extraction rises above the authored moderate value; if cost tracks delivered competence, the extraction is largely coordination cost and the tangled_rope reading is reinforced against the snare alternative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formation_cost_necessity, empirical, 'Whether institutional formation cost is competence-cost or arbiter rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(classical_latin_standard__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cls_continuity_reading_tr_t0, classical_latin_standard__continuity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cls_continuity_reading_tr_t0, observed).
narrative_ontology:measurement(cls_continuity_reading_tr_t20, classical_latin_standard__continuity_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(cls_continuity_reading_tr_t20, observed).
narrative_ontology:measurement(cls_continuity_reading_tr_t40, classical_latin_standard__continuity_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(cls_continuity_reading_tr_t40, observed).
narrative_ontology:measurement(cls_continuity_reading_tr_t60, classical_latin_standard__continuity_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(cls_continuity_reading_tr_t60, observed).
narrative_ontology:measurement(cls_continuity_reading_tr_t80, classical_latin_standard__continuity_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement_basis(cls_continuity_reading_tr_t80, observed).
narrative_ontology:measurement(cls_continuity_reading_tr_t100, classical_latin_standard__continuity_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement_basis(cls_continuity_reading_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cls_continuity_reading_be_t0, classical_latin_standard__continuity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(cls_continuity_reading_be_t0, observed).
narrative_ontology:measurement(cls_continuity_reading_be_t20, classical_latin_standard__continuity_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement_basis(cls_continuity_reading_be_t20, observed).
narrative_ontology:measurement(cls_continuity_reading_be_t40, classical_latin_standard__continuity_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(cls_continuity_reading_be_t40, observed).
narrative_ontology:measurement(cls_continuity_reading_be_t60, classical_latin_standard__continuity_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(cls_continuity_reading_be_t60, observed).
narrative_ontology:measurement(cls_continuity_reading_be_t80, classical_latin_standard__continuity_reading, base_extractiveness, 80, 0.4).
narrative_ontology:measurement_basis(cls_continuity_reading_be_t80, observed).
narrative_ontology:measurement(cls_continuity_reading_be_t100, classical_latin_standard__continuity_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement_basis(cls_continuity_reading_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cls_continuity_reading_su_t0, classical_latin_standard__continuity_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(cls_continuity_reading_su_t0, observed).
narrative_ontology:measurement(cls_continuity_reading_su_t20, classical_latin_standard__continuity_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement_basis(cls_continuity_reading_su_t20, observed).
narrative_ontology:measurement(cls_continuity_reading_su_t40, classical_latin_standard__continuity_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement_basis(cls_continuity_reading_su_t40, observed).
narrative_ontology:measurement(cls_continuity_reading_su_t60, classical_latin_standard__continuity_reading, suppression_requirement, 60, 0.34).
narrative_ontology:measurement_basis(cls_continuity_reading_su_t60, observed).
narrative_ontology:measurement(cls_continuity_reading_su_t80, classical_latin_standard__continuity_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement_basis(cls_continuity_reading_su_t80, observed).
narrative_ontology:measurement(cls_continuity_reading_su_t100, classical_latin_standard__continuity_reading, suppression_requirement, 100, 0.15).
narrative_ontology:measurement_basis(cls_continuity_reading_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(classical_latin_standard__continuity_reading, information_standard).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__reconstruction_reading).
narrative_ontology:affects_constraint(classical_latin_standard__continuity_reading, classical_latin_standard__hybrid_reading).

% DUAL FORMULATION NOTE:
% The kernel classical_latin_standard decomposes into three readings with structurally distinct constraints: continuity_reading (this story — arbiter is living transmitted usage; minimal victims; moderate gatekeeping extraction; low suppression), reconstruction_reading (arbiter is the philologically recoverable Classical form; post-Classical practice becomes the victim set; suppression of drift is high; epsilon higher), and hybrid_reading (split arbiter: textual fidelity plus development-recognition; intermediate victim set and epsilon). The epsilon values differ because the beneficiary/victim structures differ — three constraints, not one constraint with a measurement parameter. The dependency structure runs both ways across the family: the continuity reading's transmitted corpus is the evidentiary base the reconstruction reading mines, and the humanist challenge (reconstruction) pushed the continuity standard to absorb Ciceronian norms while remaining continuous.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
