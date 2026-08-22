% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Christological Doctrine: Christ's Full Divine Equality
 *   domain: historical_theology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The homoousios reading instantiates the Nicene council's pronouncement
 *   that Christ possesses identical divine substance (homoousios) with the
 *   Father—a claim that enforces doctrinal uniformity through imperial and
 *   ecclesiastical coercion against competing readings, especially the
 *   homoiousios position held by Arian communities and regional authorities.
 *   This constraint is one reading of the contested kernel 'Nicene
 *   Christological Kernel'. The sibling homoiousios_reading instantiates the
 *   alternative claim that Christ possesses similar but ontologically
 *   distinct substance (homoiousios), preserving a divine hierarchy to
 *   maintain monotheistic clarity. The homoousios reading operates as
 *   high-extraction constraint: it benefits imperial ecclesiastical
 *   consolidation and episcopal institutional power while suppressing
 *   regional theological autonomy, Arian Christian communities, and Gothic
 *   Christian networks. The enforcement machinery accelerates from Nicaea
 *   (325) through Theodosius I's edicts (late 370s–380s), systematizing
 *   suppression of heresy through property confiscation, exile of bishops,
 *   and ordination delegitimization. The claim/metric gap is deliberate: the
 *   constraint is formally CLAIMED as tangled_rope (genuine coordination
 *   problem + asymmetric extraction, requiring active enforcement), but the
 *   measurement series documents substantial extraction accumulation, rising
 *   suppression requirement, and increasing theater (defensive doctrinal
 *   elaboration growing as a share of enforcement activity relative to real
 *   pastoral coordination).
 *
 * KEY AGENTS:
 *   - Nicene Episcopal Authority (institutional agenda-setter): Bishop of Rome, Nicene council conveners, imperial ecclesiastical apparatus — sets and enforces homoousios standard; controls ordination legitimacy and church property access.
 *   - Imperial Ecclesiastical Consolidation (institutional beneficiary): Byzantine imperial court (Constantine through Theodosius I) — uses doctrinal uniformity as administrative unity lever; redirects Arian church property to imperial treasury; strengthens emperor's claim to cosmic order through unified faith.
 *   - Arian Christian Communities (moderate payer, identity-locked exit): Eastern Mediterranean, North African, and Gothic Christians holding homoiousios or subordinationist Christologies — subject to anathema, exile of leadership, property confiscation; cannot exit without renouncing constitutive theological identity.
 *   - Regional Theological Autonomy (powerful payer, constrained exit): Eastern and North African bishopric networks (Eusebius of Caesarea, regional sees) — subordinated to imperial-backed Nicene standard; pressure to endorse or face removal and confiscation.
 *   - Eastern Gothic Arian Networks (moderate payer, trapped exit): Germanic Christian communities evangelized through Ulfilas's Arian Gothic Bible — face coordinated imperial and ecclesiastical pressure; cannot exit without cultural and religious dissolution.
 *   - Dissenting Theological Voices (powerful excluded): Arius, Eusebius of Caesarea, later Aetius, Eunomius — systematically excluded from council and doctrinal authority; writings burned; exiled. Would argue homoousios over-extends Platonic vocabulary and collapses ontological distinction needed for coherent monotheism.
 *   - Nicene Theological Defenders (powerful beneficiary/agenda-setter): Athanasius, later Cappadocians (Gregory of Nazianzus, Gregory of Nyssa, Basil) — gain institutional authority and legitimacy through aligning with enforcement; develop sophisticated theological defenses of homoousios deployed against Arian arguments.
 *   - Christian Faithful in Contested Regions (powerless payer/beneficiary): Lay Christians in Arian-Nicene overlap zones — navigate conflicting episcopal authority; benefit from unified doctrine as stability or lose access to familiar theology depending on local enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.82).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.88).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Christological Doctrine: Christ's Full Divine Equality").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "historical_theology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, 'd5fb9eab-b091-4735-aa3d-11dc1a3176e7').
narrative_ontology:cs_kernel_codification('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', formalized).
narrative_ontology:cs_authority_grounding('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', extraction).
narrative_ontology:cs_interpretation_layer_present('d5fb9eab-b091-4735-aa3d-11dc1a3176e7').
narrative_ontology:cs_reading_relation('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', nicene_christological_kernel__homoiousios_reading, coexists_with).
narrative_ontology:cs_axiom('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', foundational, christ_ontological_identity_with_father).
narrative_ontology:cs_axiom_status(christ_ontological_identity_with_father, holdable).
narrative_ontology:cs_axiom_grounding('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', christ_ontological_identity_with_father, deontological).
narrative_ontology:cs_axiom('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', foundational, imperial_ecclesiastical_uniform_doctrine_mandate).
narrative_ontology:cs_axiom_status(imperial_ecclesiastical_uniform_doctrine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', imperial_ecclesiastical_uniform_doctrine_mandate, conventional).
narrative_ontology:cs_axiom('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', secondary, homoousios_platonic_vocabulary_legitimacy).
narrative_ontology:cs_axiom_status(homoousios_platonic_vocabulary_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', homoousios_platonic_vocabulary_legitimacy, empirically_contingent).
narrative_ontology:cs_reference_frame('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', christ_identical_divine_substance_with_father).
narrative_ontology:cs_drift_state('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', late_fourth_century_theodosius_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d5fb9eab-b091-4735-aa3d-11dc1a3176e7', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_episcopal_authority).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_consolidation).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, arian_christian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_theological_autonomy).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, eastern_gothic_arian_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_theological_defenders).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, christian_faithful_contested_regions).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_secular_authority).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, christian_faithful_contested_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Nicene council apparatus and the Bishop of Rome, backed by imperial ecclesiastical power, set and enforce the homoousios formulation as binding orthodoxy. They control access to church property, ordination legitimacy, and doctrinal authority. They justify the constraint as preserving Christ's divinity against subordinationist error and maintaining Christian coherence.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_episcopal_authority, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% The Byzantine imperial apparatus uses doctrinal uniformity as a tool of territorial and administrative consolidation. Homoousios orthodoxy becomes a loyalty test and legitimacy marker. The imperial treasury benefits from redirected Arian church property. Uniform doctrine strengthens the emperor's claim to cosmic order.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_consolidation, beneficiary,
    institutional, civilizational, arbitrage, universal).

% Theological communities in the Eastern Mediterranean, North Africa, and later Gothic regions holding homoiousios or subordinationist Christologies. After Nicaea, subject to anathema, exile of leadership, property confiscation, and suppression of ordination. Their reading preserves ontological distinction to maintain monotheistic clarity. Exit means renouncing their central theological commitment; their identity as Christians is constituted through this reading.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, arian_christian_communities, payer,
    moderate, biographical, identity_locked, regional).

% Eastern and North African bishopric networks, including Eusebius of Caesarea and regional sees, that held substantial theological authority and independence before Nicaea. Homoousios enforcement subordinates regional theological judgment to the imperial-backed standard. Bishops face pressure to endorse homoousios or be removed and have their property confiscated. Regional authority is progressively absorbed into imperial orthodoxy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, regional_theological_autonomy, payer,
    powerful, generational, constrained, regional).

% Germanic Christian communities (Goths, Vandals) evangelized by Arians, especially through Ulfilas's Gothic Bible translation. They face coordinated imperial and ecclesiastical pressure to abandon Arian theology. Their faith is constituted through an Arian reading and their texts are in that tradition. Exit means cultural and religious dissolution.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, eastern_gothic_arian_networks, payer,
    moderate, generational, trapped, regional).

% Bishops and theologians (Arius, Eusebius of Caesarea, later Aetius, Eunomius) who advocate homoiousios or subordinationist readings. Systematically excluded from council participation and doctrinal authority. Their writings are burned; they are exiled. They would argue that ontological distinction preserves monotheistic clarity and that homoousios over-extends Platonic language into divine mystery.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, dissenting_theological_voices, excluded,
    powerful, biographical, trapped, regional).

% Church Fathers and theologians (Athanasius, later the Cappadocians: Gregory of Nazianzus, Gregory of Nyssa, Basil of Caesarea) who defend and elaborate homoousios doctrine. Gain institutional authority, ordination legitimacy, and property control through aligning with enforcement. Develop sophisticated theological defenses deployed against Arian arguments.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_theological_defenders, beneficiary,
    powerful, civilizational, arbitrage, universal).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, nicene_theological_defenders, agenda_setter).

% Lay Christians in regions where Arian and Nicene communities coexist (Egypt, North Africa, parts of the Eastern Mediterranean). They navigate conflicting episcopal authority and doctrinal claims. Some benefit from unified doctrine as institutional stability; others lose access to familiar theology when enforcement moves against Arian clergy.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, christian_faithful_contested_regions, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, christian_faithful_contested_regions, beneficiary).

% The Byzantine imperial court (Constantine through Theodosius I) uses doctrinal enforcement as a tool of territorial control and administrative legitimacy. Uniforms the church across provinces; redirects Arian church property to the imperial treasury and favored sees; binds bishops to imperial order through orthodoxy oaths.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, imperial_secular_authority, beneficiary,
    institutional, generational, arbitrage, universal).

% The machinery of ecumenical councils (Nicaea 325, Constantinople 381, etc.) that convenes, deliberates, and pronounces binding doctrine. Staffed by imperial-selected bishops; controlled by imperial ecclesiastical policy. The council is the formal agent of enforcement, operating within imperial logistical and coercive backing.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, ecumenical_council_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, universal).

% The abstract capacity for multiple Christian communities to hold distinct Christological readings without institutional suppression. Homoousios enforcement collapses this diversity into an orthodoxy/heresy binary.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, theological_diversity, payer,
    moderate, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nicene_christological_kernel__homoousios_reading, theological_diversity).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_consolidation).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified doctrinal confession of Christ's divinity across dispersed Christian communities to resolve the problem of Christological fragmentation after Constantine's legalization and patronage of Christianity. Provides a single-language formulation (homoousios) that purports to settle the substance question and bind believers to a common faith statement.
% TRANSFER_FUNCTION: Transfers ecclesiastical authority and property rights from regional and autonomous theological voices to the imperial-backed Nicene apparatus. Redirects church property from Arian communities to Nicene sees through confiscation. Moves doctrinal pronouncement authority from local bishops to the ecumenical apparatus and imperial governance. Suppresses competing theological readings and ordination legitimacy of their clergy.
% ABSENT_VOICES: Arian bishops, Gothic theological networks (especially Ulfilas's tradition), North African regional authorities holding homoiousios readings, and dissenting theologians (Arius, Eusebius of Caesarea, later Aetius, Eunomius) are systematically excluded from council participation and authority after Nicaea. They are anathematized and their writings destroyed. They would argue that homoousios over-extends Platonic vocabulary into divine mystery, risks Sabellian modalism, and unnecessarily collapses the ontological distinction required for coherent monotheism.
% DISAPPEARANCE_RATIONALE: If homoousios enforcement vanished, Arian and homoiousios theological communities would re-emerge and re-establish their liturgical authority in contested regions (as they partially did during Constantius II's reign when enforcement wavered). The Eastern church would fragment into regional doctrinal autonomy rather than remaining under unified imperial orthodoxy. Imperial administrative control of the church would weaken. Gothic and other peripheral Christian networks would retain their Arian liturgical identity rather than being absorbed into Nicene uniformity.
% FOUNDING_PROBLEM: After Constantine's legalization and patronage of Christianity, the church faced rapid growth and regional fragmentation. Christological disputes erupted: how is Christ related to the Father? Different regions produced different confessions, threatening Christian identity and imperial administrative unity. Arius and his followers proposed a reading preserving monotheistic clarity by distinguishing Christ as subordinate; this collided with bishops who saw it as diminishing Christ's divinity. Nicaea (325 CE) was called to impose a binding formulation to resolve the doctrinal fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: The imperial court and Nicene bishops attest the founding problem remains live: without uniform doctrine, the church fragments and imperial legitimacy weakens. Historians and modern theologians confirm that Christological fragmentation WAS a real problem in the early fourth century, but contest whether homoousios was structurally necessary. Arian communities and later historical sources document that regional Arian doctrine maintained communion and theological coherence without homoousios. No corroboration from outside the Nicene beneficiary set attests that homoousios enforcement was mandatory rather than one choice among coherent alternatives.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 (initial Nicene pronouncement) to 0.82 (Theodosius-era systematic enforcement) because the constraint's persistence becomes increasingly dependent on coercive enforcement rather than participant buy-in. Initial Nicene formulation (325) presented homoousios as a resolution to a real coordination problem (Christological fragmentation post-Constantine), so early extractiveness is moderate—genuine coordination content is present. But as enforcement accelerates through the 330s–350s (Constantius II's reign), extractiveness rises sharply: suppression becomes the mechanism of persistence rather than persuasion. The theater ratio rises from 0.18 to 0.41, indicating that a growing share of enforcement activity consists of doctrinal elaboration and theological defense (theater) rather than solving the foundational coordination problem. Early-period activity is pastoral coordination and catechesis; late-period activity is increasingly doctrinal policing and heresy suppression—the same enforcement machinery deployed to preserve a fixed formula rather than to coordinate Christian belief across diversity. Suppression requirement climbs from 0.62 to 0.88 because the constraint encounters real, organized resistance from Arian communities and regional authorities; enforcement must intensify to maintain the homoousios standard as competing readings re-emerge and gain followers (notably during Constantius II's Arian-favorable reign, when Nicene enforcement wavered). The measurement series uses a shared time grid (all three metrics measured at all six time points: 325, 333, 341, 353, 365, 381) to enable temporal lifecycle analysis. The interval from 325 (Nicaea) to 381 (Constantinople I, which reaffirmed and systematized homoousios enforcement) captures the formation and hardening of the enforcement infrastructure.
 *
 * PERSPECTIVAL GAP:
 *   From the Nicene episcopal-authority seat and the imperial seat, this constraint solves a real coordination problem (doctrinal fragmentation threatens Christian identity and imperial administrative coherence) and operates as coordination requiring active enforcement to prevent defection back to competing readings. From the Arian community and regional-autonomy seats, the constraint operates as enforced extraction: a formulaic uniformity imposed from above that suppresses legitimate theological diversity and subordinates local authority to imperial-ecclesiastical power. From the Gothic Christian seat, the constraint is experienced as cultural and religious invasion—their Arian tradition and Ulfilas's Gothic Bible are declared heretical and suppressed. The engine computes directionality from the structural data: the agenda_setter seat (Nicene authority) has low d (beneficiary, arbitrage exit); the payer seats (Arians, regional bishops, Goths) have high d (trapped or identity-locked exit, bearing suppression). The measurement series documents why this perspectival gap persists and widens: extractiveness and suppression rise over time, indicating the constraint's persistence increasingly depends on coercive enforcement rather than coordination benefit felt across all seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Nicene episcopal authority: d ≈ 0.1 (beneficiary—collects authority and property; arbitrage exit: they can shift positions if political winds change; institutional power). Imperial ecclesiastical consolidation: d ≈ 0.15 (beneficiary—gains administrative leverage and property; arbitrage exit: empire can shift religious policy). Arian Christian communities: d ≈ 0.88 (target—suppressed through anathema and confiscation; identity_locked exit: cannot renounce faith without dissolving self-identity; moderate power means limited legal recourse). Regional theological autonomy: d ≈ 0.85 (target—subordinated to imperial standard; constrained exit: resignation or exile are the options). Eastern Gothic networks: d ≈ 0.92 (target—trapped exit, cultural dissolution if they abandon Arian tradition; moderate power provides no protection against imperial coercion). Nicene theological defenders: d ≈ 0.05 (beneficiary—gain institutional authority; arbitrage exit: they support the winning side). Christian faithful in contested regions: d ≈ 0.55 (mixed: genuinely coordinated in stable regions under one authority; but trapped where competing episcopal authorities coerce local populations; powerless exit options). The directionality overrides are not needed; the structural derivation from beneficiary/victim + exit + power captures the asymmetry accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (decay of founding mandate) appears contested but nascent during the 325–381 interval. The founding problem (Christological fragmentation post-Constantine) is real at origin and Nicaea genuinely aims to solve it through binding confession. However, by the mid-fourth century (353 CE forward), the constraint persists increasingly through enforcement against Arian persistence and regional resistance rather than through widespread participant commitment. The theater ratio (defensive theological elaboration) rises, indicating the foundational coordination problem is being solved (Christians increasingly do accept homoousios or at least internalize it as 'orthodox'), but the constraint persists because enforcement now has inertia independent of the problem it was built to address. The founding_problem_status='contested' reflects this: the imperial Nicene authority attests the problem remains live (fragmentation risk, heresy threat), but historians and theological reflection document that the problem could have been managed through non-homoousios coordinations (Arian communities maintained coherent faith and communion without it). The measurement series shows extractiveness and suppression rising while early-period coordination benefit is assumed solved, which is the classic mandate-decay signal: the machinery is solving its founding problem but persisting through institutional inertia and enforcement expansion. By 381 (Constantinople I), homoousios is institutionally locked and the constraint has shifted from coordination-with-enforcement to pure-enforcement-with-theater-preservation. The computed type at t=381 should be tangled_rope or snare, depending on whether the residual coordination function remains substantive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    homoousios_vs_homoiousios_logical_difference,
    'Does homoousios logically foreclose homoiousios, or are they genuinely coexisting alternative readings of Christ''s ontological status?',
    'Systematic theological analysis: if homoousios and homoiousios can both be defended within orthodox Christian doctrine without internal contradiction (both preserve Christ''s divinity and monotheism), they coexist; if one necessarily implies the denial of the other''s core claim, they foreclose.',
    'If they coexist, the Nicene enforcement is institutional coercion choosing one reading over another—pure extraction. If homoousios forecloses homoiousios, then the constraint solves a real theological problem and enforcement is coordination-with-coercion (tangled_rope). The measurement trajectory (rising extractiveness, rising theater) suggests coexistence, implying the constraint hardens from early-stage coordination toward extraction-with-inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoousios_vs_homoiousios_logical_difference, conceptual, 'Whether homoousios and homoiousios are logically foreclosing or coexisting readings.').

omega_variable(
    coordinating_function_vs_extraction_function_boundary,
    'Could the founding problem (Christological fragmentation post-Constantine) have been solved through non-enforced coordination or through a different formula that did not require suppression of Arian communities?',
    'Counterfactual historical analysis: examine periods when Arian and Nicene communities coexisted without enforcement (e.g., Constantius II''s reign, when enforcement wavered), and assess whether Christian communion and theological coherence were maintained. Also examine regions where homoiousios remained dominant (Gothic, parts of the East) and assess whether fragmentation resulted or whether stable alternative communions existed.',
    'If stable alternatives could coexist without enforcement, then homoousios extraction is separable from the founding coordination function—the constraint could solve the fragmentation problem without suppressing Arians, and the measured extraction reflects enforcement that goes beyond what coordination requires. If Christological division inevitably produced institutional fragmentation regardless of formula chosen, then some level of enforcement is intrinsic to coordination and the extraction/coordination boundary is blurred.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordinating_function_vs_extraction_function_boundary, empirical, 'Whether enforced uniformity was structurally necessary or politically chosen.').

omega_variable(
    identity_locked_exit_homoousios_victims,
    'For Arian communities coded as identity_locked (exit means renouncing constitutive theological belief), does identity-lock persist after enforcement ceases, or does it dissolve?',
    'Historical/ethnographic observation: when homoousios enforcement wavered (Constantius II''s reign), did Arian communities maintain their faith commitment, or did they shift to Nicene confession opportunistically? When homoousios enforcement resumed (Theodosius I onward), did formerly suppressed Arians re-emerge with their faith intact, or did they show signs of internalized suppression that persisted after enforcement ended?',
    'If identity-lock persists after enforcement ceases, the suppression is structurally embedded in the reading itself and identity fusion is deep. If identity-lock dissolves or weakens after enforcement ceases, the suppression is mostly external structural coercion, and post-exit recovery is possible. This determines whether the constraint''s effective suppression is higher than the base-properties scalar suggests (internalized suppression multiplies structural suppression).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_homoousios_victims, empirical, 'Whether suppression of Arian identity-locked communities is structural or internalized.').

omega_variable(
    reading_contest_as_kernel_fidelity,
    'Is the homoousios/homoiousios contest a genuine dispute about the meaning and proper transmission of the Nicene Kernel, or is it a case of institutional capture where homoousios actors claimed the kernel exclusively to suppress alternatives?',
    'Genealogical analysis: trace the theological development from Council of Nicaea through Fourth Century councils (Antioch, Sirmium, Constantinople). Did homoousios emerge as a consensus-refined reading of Nicaea, or was it imposed by a faction that then claimed it was always Nicaea''s intent? Do surviving pre-Nicene Christian sources show homoousios was a live option before the council, or was it introduced as a new Platonic term?',
    'If homoousios emerges as genuine refinement and consensus, the kernel supports its reading as legitimate transmission. If it emerges as factional capture (homoousios actors claimed the kernel exclusively to defeat Arians), then the constraint is extractive misuse of authority to suppress theological diversity—the mandate becomes mandatrophy (enforcing a reading as ''the kernel''s only true meaning'' when the kernel was genuinely ambiguous).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_as_kernel_fidelity, conceptual, 'Whether homoousios represents genuine kernel transmission or factional capture.').

omega_variable(
    imperial_consolidation_structural_necessity,
    'Did Christian doctrinal uniformity genuinely require enforcement to achieve, or was imperial enforcement chosen as an efficient path even though doctrinal coordination could have happened through persuasion and extended dialogue?',
    'Comparative historical analysis: examine how later doctrinal settlements (Ephesus 431, Chalcedon 451) were achieved and whether alternative paths to settlement existed. Also examine non-Christian cases of dispersed communities achieving doctrinal coordination without centralized coercion—do examples exist that suggest coordination is possible without enforcement?',
    'If enforcement was structurally necessary because communities had incompatible commitments that could not resolve through dialogue, then the constraint solves a real coordination problem and represents genuine tangled_rope (coordination requiring enforcement). If enforcement was chosen for efficiency and control rather than necessity, then the extraction from Arian communities is instrumental to imperial consolidation, not intrinsic to solving fragmentation—the constraint is closer to snare than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_consolidation_structural_necessity, empirical, 'Whether doctrinal uniformity enforcement was structurally necessary or politically chosen for administrative efficiency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_christological_kernel__homoousios_reading, theater_ratio, 325, 0.18).
narrative_ontology:measurement(nice_tr_t333, nicene_christological_kernel__homoousios_reading, theater_ratio, 333, 0.24).
narrative_ontology:measurement(nice_tr_t341, nicene_christological_kernel__homoousios_reading, theater_ratio, 341, 0.31).
narrative_ontology:measurement(nice_tr_t353, nicene_christological_kernel__homoousios_reading, theater_ratio, 353, 0.37).
narrative_ontology:measurement(nice_tr_t365, nicene_christological_kernel__homoousios_reading, theater_ratio, 365, 0.39).
narrative_ontology:measurement(nice_tr_t381, nicene_christological_kernel__homoousios_reading, theater_ratio, 381, 0.41).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_christological_kernel__homoousios_reading, base_extractiveness, 325, 0.58).
narrative_ontology:measurement(nice_be_t333, nicene_christological_kernel__homoousios_reading, base_extractiveness, 333, 0.65).
narrative_ontology:measurement(nice_be_t341, nicene_christological_kernel__homoousios_reading, base_extractiveness, 341, 0.72).
narrative_ontology:measurement(nice_be_t353, nicene_christological_kernel__homoousios_reading, base_extractiveness, 353, 0.79).
narrative_ontology:measurement(nice_be_t365, nicene_christological_kernel__homoousios_reading, base_extractiveness, 365, 0.81).
narrative_ontology:measurement(nice_be_t381, nicene_christological_kernel__homoousios_reading, base_extractiveness, 381, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_christological_kernel__homoousios_reading, suppression_requirement, 325, 0.62).
narrative_ontology:measurement(nice_su_t333, nicene_christological_kernel__homoousios_reading, suppression_requirement, 333, 0.71).
narrative_ontology:measurement(nice_su_t341, nicene_christological_kernel__homoousios_reading, suppression_requirement, 341, 0.78).
narrative_ontology:measurement(nice_su_t353, nicene_christological_kernel__homoousios_reading, suppression_requirement, 353, 0.82).
narrative_ontology:measurement(nice_su_t365, nicene_christological_kernel__homoousios_reading, suppression_requirement, 365, 0.86).
narrative_ontology:measurement(nice_su_t381, nicene_christological_kernel__homoousios_reading, suppression_requirement, 381, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nicene_christological_kernel__homoousios_reading, 0.18).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel__homoiousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel__homoousios_imperial_enforcement_machinery).

% DUAL FORMULATION NOTE:
% The Nicene Christological Kernel decomposes into (at minimum) two structurally distinct readings with markedly different ε values: the homoousios reading (this story, ε=0.82, enforces uniformity through coercion, high extraction) and the homoiousios reading (sibling story, ε lower, preserves diversity and regional autonomy). The readings share a common kernel (the contested claim about Christ's ontological status) but diverge radically in mechanism, beneficiary structure, and victims. Homoousios enforces imperial ecclesiastical consolidation through suppression; homoiousios preserves regional theological autonomy and Arian Christian communities. The two readings coexist_with each other—different parties hold them simultaneously—rather than one foreclosing the other logically. They are linked through network.affects_constraints: homoousios influences homoiousios by creating institutional pressure (enforcement, confiscation, exile) that forces homoiousios communities underground or into exit. A third related constraint captures the imperial enforcement machinery itself as a separate object (nicene_christological_kernel__homoousios_imperial_enforcement_machinery), distinguishing the theological claim (homoousios substance doctrine) from the institutional apparatus that enforces it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
