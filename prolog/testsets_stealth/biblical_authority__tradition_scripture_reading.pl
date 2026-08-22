% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_biblical_authority__tradition_scripture_reading, []).

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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Guardianship of Scriptural Interpretation (Tradition-and-Scripture Reading of Biblical Authority)
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   biblical_authority: the tradition-and-scripture reading, on which
 *   authoritative interpretation of scripture requires tradition and the
 *   magisterium guards the deposit of faith. The kernel label decomposes into
 *   three structurally distinct constraints: sola_scriptura_reading (no
 *   clerical interpretive monopoly, fragmentation cost externalized),
 *   conciliar_reading (councils and patristic consensus adjudicate, no
 *   magisterial decree), and this reading (magisterial guardianship,
 *   centralized adjudication, sacramental mediation). Each is a separate
 *   constraint with its own epsilon, beneficiaries, and victims; this story
 *   authors epsilon for the magisterial arrangement ONLY, never for the
 *   sola-scriptura or conciliar alternatives and never averaged across
 *   readings. The interval 1546-1965 covers the codified contest era, from
 *   Trent's decree on scripture and tradition through Vatican II's Dei
 *   Verbum; the reading's genealogy (patristic rule of faith, medieval canon
 *   law) predates the interval. Claimed type and metrics are authored
 *   independently: the arrangement is claimed as a tangled rope, a genuine
 *   coordination function (doctrinal unity, deposit preservation, sacramental
 *   continuity) carrying asymmetric transfer (interpretive authority and
 *   material support move from laity to hierarchy, enforced against
 *   alternatives), while the metrics describe its actual operation across the
 *   interval.
 *
 * KEY AGENTS:
 *   - clerical_hierarchy: agenda-setter and primary beneficiary (institutional/arbitrage) — writes what the tradition requires, collects assent, loyalty, and material support
 *   - ordained_clergy: secondary beneficiary (organized/identity_locked) — holds the sacramental mediation office that constitutes livelihood and identity
 *   - lay_faithful: primary target (powerless/constrained) — owes assent, receives doctrine and sacraments only through the gate, bears exit costs
 *   - vernacular_bible_translators: historical target (moderate/trapped) — persecuted for unauthorized vernacular scripture
 *   - dissenting_theologians: secondary target (moderate/identity_locked) — censured from within, bound by vocational identity
 *   - reformers_and_protestant_readers: excluded objector (organized/mobile) — holds the sibling reading; sits outside the adjudicative conversation
 *   - historians_of_christianity: analytical observer (analytical/analytical) — documents the record against the arrangement's self-description
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.58).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.42).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Guardianship of Scriptural Interpretation (Tradition-and-Scripture Reading of Biblical Authority)").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, 'a7ce5f8f-edce-452e-bac7-49f412163b1b').
narrative_ontology:cs_kernel_codification('a7ce5f8f-edce-452e-bac7-49f412163b1b', fixed_text).
narrative_ontology:cs_authority_grounding('a7ce5f8f-edce-452e-bac7-49f412163b1b', lineage).
narrative_ontology:cs_interpretation_layer_present('a7ce5f8f-edce-452e-bac7-49f412163b1b').
narrative_ontology:cs_reading_relation('a7ce5f8f-edce-452e-bac7-49f412163b1b', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('a7ce5f8f-edce-452e-bac7-49f412163b1b', biblical_authority__conciliar_reading, forecloses).
narrative_ontology:cs_axiom('a7ce5f8f-edce-452e-bac7-49f412163b1b', foundational, tradition_necessary_for_authoritative_interpretation).
narrative_ontology:cs_axiom_status(tradition_necessary_for_authoritative_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('a7ce5f8f-edce-452e-bac7-49f412163b1b', tradition_necessary_for_authoritative_interpretation, theological).
narrative_ontology:cs_axiom('a7ce5f8f-edce-452e-bac7-49f412163b1b', foundational, magisterium_definitive_adjudicator_of_deposit).
narrative_ontology:cs_axiom_status(magisterium_definitive_adjudicator_of_deposit, holdable).
narrative_ontology:cs_axiom_grounding('a7ce5f8f-edce-452e-bac7-49f412163b1b', magisterium_definitive_adjudicator_of_deposit, theological).
narrative_ontology:cs_reference_frame('a7ce5f8f-edce-452e-bac7-49f412163b1b', apostolic_deposit_magisterial_guardianship).
narrative_ontology:cs_drift_state('a7ce5f8f-edce-452e-bac7-49f412163b1b', post_dei_verbum_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a7ce5f8f-edce-452e-bac7-49f412163b1b', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, ordained_clergy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_faithful).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, vernacular_bible_translators).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, dissenting_theologians).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, apostolic_succession_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, deposit_of_faith_doctrine).
narrative_ontology:constraint_vindicates(biblical_authority__tradition_scripture_reading, development_of_doctrine_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines what the deposit of faith contains and what it requires, issues definitive teaching, adjudicates disputes over scripture and doctrine, and confirms or rejects councils, translations, and theological works. It writes the rules by which interpretation is authorized and collects the assent, institutional loyalty, and material support that flow to the teaching office. Its way out of the arrangement is not departure but re-framing: it holds the authority to redefine what the tradition requires, as Vatican I and Vatican II each did.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, clerical_hierarchy, agenda_setter,
    institutional, generational, arbitrage, global).

% Administer the sacraments by exclusive warrant and preach with delegated teaching authority. Livelihood, social standing, and vocational identity are constituted by the mediation office: without the requirement that grace and doctrine pass through ordained hands, the office's economic and social basis would not exist as it does. Leaving means laicization, a costly and stigmatized rupture with the community and the self the office built.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, ordained_clergy, beneficiary,
    organized, biographical, identity_locked, regional).

% Receive doctrine as taught, owe assent of intellect and will to definitive teachings, and access the sacraments only through clerical mediation. Their own readings of scripture carry no standing in adjudication: where a lay reading diverges from magisterial teaching, the divergence is error by definition. Leaving means forfeiting sacramental participation, community, and, in the arrangement's own terms, the ordinary means of grace; family and cultural embeddedness raise the price further.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_faithful, payer,
    powerless, biographical, constrained, global).

% Produced scripture in vernacular languages so laypeople could read it without clerical intermediation. The arrangement treated unauthorized translation as a threat to the guarded deposit: Arundel's Constitutions required license for new translations, Tyndale was strangled and burned in 1536, and unapproved translations appeared on the Index. Once committed to the work, translators faced recantation or prosecution, and the machinery pursued them across borders.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, vernacular_bible_translators, payer,
    moderate, biographical, trapped, continental).

% Catholic scholars who publish on scripture and doctrine under review by the teaching office. Where their conclusions diverge from definitive teaching they face censure, withdrawal of mandatum or missio canonica, or removal from Catholic institutions. Their training, vocation, and professional standing are formed inside the tradition they dispute, so censure typically binds them tighter rather than releasing them; they contest from within.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, dissenting_theologians, payer,
    moderate, biographical, identity_locked, global).

% Hold the sibling reading that scripture alone suffices for authoritative interpretation. Their objection to magisterial guardianship exists inside the arrangement only as the error Trent anathematized; they sit outside the adjudicative conversation entirely. Their exit was exercised, founding parallel communions, and the enforcement apparatus existed in significant part to keep the boundary they crossed closed.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, reformers_and_protestant_readers, excluded,
    organized, generational, mobile, continental).

% Document the arrangement's operation across the interval from outside the conversation: council decrees, Index lists, censorship cases, censure files, and conciliar texts. They take no seat in the adjudication and collect nothing from it; their record is the account against which the arrangement's self-description can be checked.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, historians_of_christianity, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, clerical_hierarchy).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation problem: a single teaching office determines what the deposit contains, adjudicates competing readings of scripture, and maintains doctrinal continuity across generations and cultures. Canon determination, creed definition, dispute resolution, and sacramental uniformity are each solved once centrally rather than per-reader.
% TRANSFER_FUNCTION: Moves interpretive authority and adjudication from lay readers and local communities to the clerical hierarchy; moves material support (tithes, offerings, historically sacramental fees and indulgence payments) from laity to clergy; moves doctrinal certainty and sacramental access from the institution downward to the faithful.
% ABSENT_VOICES: The reformers and Protestant readers are absent by design: their objection exists inside the arrangement only as anathematized error. Vernacular translators were absent by execution and Index listing. Censured theologians are present but bound. Orthodox conciliar voices have been absent from the Western adjudicative conversation since 1054. Each would contest the premise that authoritative interpretation requires magisterial decree; none sits where the decision is made.
% DISAPPEARANCE_RATIONALE: Without the tradition requirement and the magisterial guard, scriptural interpretation would devolve to private judgment and competing councils on the Protestant pattern: doctrinal fragmentation, parallel communions, and sacramental systems without a common warrant. The hierarchy's teaching office would lose its object, since there would be no deposit to guard authoritatively, and the largest Christian communion's unity machinery would reorganize around whatever interpretive authorities replaced it.
% FOUNDING_PROBLEM: Early Christianity faced an interpretive crisis with no settled machinery: proliferating gospels and gnostic readings, no fixed canon, and heresiological conflicts (Marcion, Arius) that threatened the movement's coherence. The tradition reading was built to solve this: preserve and adjudicate the apostolic deposit through a continuous teaching office rather than leaving meaning to each reader.
% FOUNDING_PROBLEM_CORROBORATION: The founding crisis is corroborated from outside the beneficiary set: secular historians of early Christianity document the canon disputes and gnostic proliferation independently of church interests, and Protestant ecumenical scholarship acknowledges interpretive fragmentation as a real problem even while rejecting the magisterial remedy. Orthodox witnesses attest the problem's reality while locating adjudication in conciliar consensus. Whether the problem remains live is disputed: sola-scriptura readers hold scripture's clarity sufficient and attribute ongoing fragmentation to human sinfulness rather than to the absence of a magisterium.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(biblical_authority__tradition_scripture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(biblical_authority__tradition_scripture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high but narrowing (peak 0.75 in 1622, 0.58 at interval end): the arrangement transfers interpretive authority wholesale from lay readers to the hierarchy and gates grace behind clerical mediation, but its extractive surface contracted after Vatican II, with vernacular scripture encouraged, censorship moribund, and dissent tolerated within bounds. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope; only extractiveness is scaled by the engine. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity change: the Index machinery built to peak (0.81, 1571), was contested through the Enlightenment, and decayed to functional abolition (0.42, 1965; formally abolished 1966, just past interval end). Theater_ratio rises modestly (0.14 to 0.31) as the enforcement apparatus becomes increasingly ceremonial while the core teaching function remains real; the theater tracks the enforcement layer's atrophy, not the teaching function's. Accessibility_collapse is 0.50: within the reading's framework the scripture-alone alternative is ruled out once the arrangement is understood, but the alternatives demonstrably persist and are livable outside it (the Reformation proved their viability), and legitimate interpretive pluralism survives below the definitive level. Resistance is 0.65: Reformation, Enlightenment, modernist crisis, and post-conciliar dissent. Identity-lock binds two seats: ordained clergy (professional and institutional identity fusion, where ordination constitutes self, standing, and livelihood, so exit means stigmatized laicization) and dissenting theologians (vocational identity fusion, where training and professional standing are formed inside the disputed tradition, so censure binds tighter). Coalition note: the lay seat is individually powerless but has episodically organized; lay biblical and theological movements contributed to the Dei Verbum opening. All three metric series run on one shared six-point grid (1546, 1571, 1622, 1750, 1870, 1965) so the engine samples every metric at every examined time.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the magisterial seat, the arrangement is the Church's own self-understanding: the guardian that delivers doctrinal certainty, sacramental grace, and unity across a global communion; the coordination is the point, and the assent it collects is the price of membership in a truth-bearing community. From the lay seat, the same structure operates as a gate they do not control: their interpretive conclusions carry no standing, grace arrives only through the gate, and the gatekeeper collects assent and support. From the translator seat, it was a prosecution machine. The engine computes per-seat classifications from the structural data; this story does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map directly onto the structure. clerical_hierarchy (beneficiary, arbitrage exit, institutional power) sits at the beneficiary end of directionality: it writes what the tradition requires and collects the assent. ordained_clergy (beneficiary, identity_locked) collect the mediation monopoly's livelihood and status. lay_faithful (victim, constrained exit, individually powerless) sit near the target end, caught between assent obligations and the price of exit. vernacular_bible_translators (victim, trapped) and dissenting_theologians (victim, identity_locked) sit nearest the full-target end: no exit, full exposure. No directionality overrides are authored: the derivation from declared beneficiary/victim structure plus exit options is expected to place each seat accurately, and the two same-power (moderate) victims, translators and theologians, genuinely share a structural position despite different eras and mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents mislabeling in both directions. A pure-extraction reading would miss the genuine coordination function: doctrinal fragmentation is a real collective-action problem, the arrangement demonstrably solves it (low doctrinal fragmentation within the communion is this reading's structural delta), and the founding problem is corroborated from outside the beneficiary set. A pure-coordination reading would miss the asymmetric transfer: the same structure that coordinates doctrine moves interpretive authority and material support from laity to hierarchy and enforced the boundary against alternatives with fire and Index. Mandatrophy is NOT declared: the founding problem, interpretive fragmentation, remains live, so the status-live x world-rearranges combination raises no zombie flag. What has atrophied is the enforcement apparatus, not the core function; the theater_ratio series tracks the former. The open question the omegas carry is whether the post-conciliar softening is a permanent narrowing of the extraction surface or a relaxation phase in a cycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_locus,
    'Is the arrangement''s extraction intrinsic to the claim that scripture requires tradition, or specific to the magisterial adjudication of tradition?',
    'Comparative structural analysis across the sibling readings: if a conciliar instantiation of tradition-necessity shows materially lower extraction with comparable doctrinal unity, the extraction attaches to the magisterial organ rather than to tradition-necessity as such.',
    'If the extraction is organ-specific, the tradition-necessity claim and the magisterial-guardianship claim are separable constraints with different classifications; if intrinsic, every tradition-requiring reading carries the extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_locus, conceptual, 'Whether extraction lives in tradition-necessity or in magisterial adjudication.').

omega_variable(
    mediation_grace_or_gate,
    'Is sacramental mediation the delivery of a good that requires mediation (grace conferred through the office) or a gate on a good available otherwise?',
    'Not resolvable by external data; the premise is theological and indexed to the framework. Comparative analysis across readings: sibling frameworks that deny mediated grace treat the same mediation as pure gating, while within this reading the mediation is constitutive of the good.',
    'If mediation is constitutive, part of the measured extraction is the price of the coordination itself and effective extraction falls toward coordination cost; if it is gating, the mediation requirement is near-pure rent and the constraint moves toward the snare end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mediation_grace_or_gate, conceptual, 'Whether the mediation requirement coordinates a mediated good or gates an unmediated one.').

omega_variable(
    suppression_structural_or_internalized,
    'After the enforcement machinery''s decay, is the residual suppression of lay interpretive agency structural (assent obligations, censure, denial of standing) or internalized (formation that renders private judgment spiritually suspect)?',
    'Post-decay engagement trajectory: if lay interpretive engagement surged once structural barriers lifted, the suppression was substantially structural; the observed partial surge (lay biblical movements, flourishing Catholic scholarship) alongside a persistent gap between permitted and actual engagement indicates a substantial internalized component.',
    'If substantially internalized, effective suppression outlasts the enforcement machinery; the constraint''s coercive force persists without enforcers and the end-state suppression value understates operative suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_or_internalized, empirical, 'Structural versus internalized suppression of lay interpretive agency.').

omega_variable(
    founding_problem_modern_dissolution,
    'Does the founding problem, interpretive fragmentation, remain live, or was it an artifact of pre-critical textual conditions (manuscript variance, no printing, no vernacular literacy) that the modern textual apparatus dissolved?',
    'Comparative doctrinal-divergence measurement across sola-scriptura communions versus the magisterial communion under modern textual conditions; if divergence persists at comparable rates, the problem is live independent of textual conditions.',
    'If the problem is dissolved, the arrangement''s coordination justification weakens toward pure extraction and the mandatrophy direction opens; if live, the coordination function holds and the tangled structure is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_modern_dissolution, empirical, 'Whether modern textual conditions dissolved the founding fragmentation problem.').

omega_variable(
    post_conciliar_softening_reversibility,
    'Is the post-conciliar narrowing of the extraction and enforcement surface a permanent structural change or a tactical relaxation within an unchanged architecture?',
    'Observe enforcement-capacity response to the next doctrinal stress event: if new censures, mandate withdrawals, or boundary enforcement rebuild under pressure, the relaxation is cyclical; if enforcement capacity stays dismantled, the change is structural.',
    'If cyclical, the end-state metrics understate steady-state extraction and the drift trajectory will oscillate rather than decline monotonically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_conciliar_softening_reversibility, empirical, 'Reversibility of the post-conciliar enforcement decay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 1546, 1965).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t1546, biblical_authority__tradition_scripture_reading, theater_ratio, 1546, 0.14).
narrative_ontology:measurement_basis(bibl_tr_t1546, observed).
narrative_ontology:measurement(bibl_tr_t1571, biblical_authority__tradition_scripture_reading, theater_ratio, 1571, 0.15).
narrative_ontology:measurement_basis(bibl_tr_t1571, observed).
narrative_ontology:measurement(bibl_tr_t1622, biblical_authority__tradition_scripture_reading, theater_ratio, 1622, 0.17).
narrative_ontology:measurement_basis(bibl_tr_t1622, observed).
narrative_ontology:measurement(bibl_tr_t1750, biblical_authority__tradition_scripture_reading, theater_ratio, 1750, 0.21).
narrative_ontology:measurement_basis(bibl_tr_t1750, observed).
narrative_ontology:measurement(bibl_tr_t1870, biblical_authority__tradition_scripture_reading, theater_ratio, 1870, 0.26).
narrative_ontology:measurement_basis(bibl_tr_t1870, observed).
narrative_ontology:measurement(bibl_tr_t1965, biblical_authority__tradition_scripture_reading, theater_ratio, 1965, 0.31).
narrative_ontology:measurement_basis(bibl_tr_t1965, observed).

% Extraction over time
narrative_ontology:measurement(bibl_be_t1546, biblical_authority__tradition_scripture_reading, base_extractiveness, 1546, 0.7).
narrative_ontology:measurement_basis(bibl_be_t1546, observed).
narrative_ontology:measurement(bibl_be_t1571, biblical_authority__tradition_scripture_reading, base_extractiveness, 1571, 0.73).
narrative_ontology:measurement_basis(bibl_be_t1571, observed).
narrative_ontology:measurement(bibl_be_t1622, biblical_authority__tradition_scripture_reading, base_extractiveness, 1622, 0.75).
narrative_ontology:measurement_basis(bibl_be_t1622, observed).
narrative_ontology:measurement(bibl_be_t1750, biblical_authority__tradition_scripture_reading, base_extractiveness, 1750, 0.7).
narrative_ontology:measurement_basis(bibl_be_t1750, observed).
narrative_ontology:measurement(bibl_be_t1870, biblical_authority__tradition_scripture_reading, base_extractiveness, 1870, 0.66).
narrative_ontology:measurement_basis(bibl_be_t1870, observed).
narrative_ontology:measurement(bibl_be_t1965, biblical_authority__tradition_scripture_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement_basis(bibl_be_t1965, observed).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t1546, biblical_authority__tradition_scripture_reading, suppression_requirement, 1546, 0.76).
narrative_ontology:measurement_basis(bibl_su_t1546, observed).
narrative_ontology:measurement(bibl_su_t1571, biblical_authority__tradition_scripture_reading, suppression_requirement, 1571, 0.81).
narrative_ontology:measurement_basis(bibl_su_t1571, observed).
narrative_ontology:measurement(bibl_su_t1622, biblical_authority__tradition_scripture_reading, suppression_requirement, 1622, 0.79).
narrative_ontology:measurement_basis(bibl_su_t1622, observed).
narrative_ontology:measurement(bibl_su_t1750, biblical_authority__tradition_scripture_reading, suppression_requirement, 1750, 0.71).
narrative_ontology:measurement_basis(bibl_su_t1750, observed).
narrative_ontology:measurement(bibl_su_t1870, biblical_authority__tradition_scripture_reading, suppression_requirement, 1870, 0.62).
narrative_ontology:measurement_basis(bibl_su_t1870, observed).
narrative_ontology:measurement(bibl_su_t1965, biblical_authority__tradition_scripture_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement_basis(bibl_su_t1965, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'biblical authority' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints: sola_scriptura_reading (no interpretive monopoly; extraction surface minimal; fragmentation cost externalized), conciliar_reading (council/consensus adjudication; no single magisterial decree), and this story's tradition_scripture_reading (magisterial guardianship; high clerical extraction; low fragmentation). The epsilon values differ because the arrangements differ; measuring 'biblical authority' through any single observable would conflate them. This story links both siblings. The family's shared substrate is the apostolic deposit and patristic witness, which every reading claims and cites, so each reading's legitimacy conditions bear structurally on the others'; the foreclosure relations are declared in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
