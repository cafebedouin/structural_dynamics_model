% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed as Binding Metaphysical Ontology with Heresy Sanction (Strict Orthodox Reading)
 *   domain: religious/doctrinal
 *
 * SUMMARY:
 *   This story instantiates the strict_orthodox_reading of the
 *   nicene_creed_authority kernel: the creed binds every believer to one
 *   metaphysical ontology, and deviation is heresy warranting sanction. The
 *   standing arrangement under contest — the referent of every metric here —
 *   is that binding-with-sanction regime as it has actually operated from
 *   Nicaea (325) to the present, assessed by this reading's own lights: the
 *   strict reading regards the discipline as largely warranted, so it authors
 *   a lower epsilon over the same referent than a symbolic or external
 *   reading would, while still acknowledging the real costs borne by
 *   condemned communities and constrained interpreters. Per the
 *   epsilon-invariance principle, the colloquial object 'the authority of the
 *   Nicene Creed' decomposes into three structurally distinct constraints —
 *   this reading, symbolic_confessional_reading, and
 *   liturgical_habituation_reading — each with its own epsilon, beneficiary
 *   structure, and classification; the siblings are separate files linked
 *   through the network. This is the only one of the three readings whose
 *   premise entails heresy policing: it sanctions the practical equivalents
 *   of its own siblings, which is why its structural delta is high
 *   extractiveness with a clear beneficiary set (hierarchical clergy) and
 *   victim set (heterodox communities, lay interpreters).
 *
 * KEY AGENTS:
 *   - episcopal_hierarchy: agenda-setter and primary beneficiary (institutional/arbitrage) — defines the creed's binding content, administers sanction, collects jurisdiction and deference
 *   - imperial_authorities: historical secondary agenda-setter and indirect beneficiary (powerful/mobile) — enforced conciliar verdicts with civil power; withdrew as church and state separated
 *   - parish_clergy: dual-positioned beneficiary-and-bearer (organized/constrained) — local custodian of the confession, subject to the same discipline it administers
 *   - heterodox_communities: full targets (powerless/trapped) — condemned positions; the sanction's object, with no licensed position inside the framework
 *   - lay_interpreters: targets with identity-locked exit (powerless/identity_locked) — assent required as a condition of belonging itself
 *   - dissenting_theologians: targets (moderate/constrained) — vocational inquiry conducted inside the communion that disciplines its conclusions
 *   - ecumenical_churches: excluded voices (powerful/mobile) — recite the same creed, deny the binding claim, defined by this reading as the deviation it polices
 *   - historian_of_doctrine: analytical observer (analytical/analytical) — sees the full machinery across the interval from outside every party's commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.6).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.5).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, tangled_rope).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed as Binding Metaphysical Ontology with Heresy Sanction (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "religious/doctrinal").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '2e874bcc-4cea-400f-803f-056c22c4a2bb').
narrative_ontology:cs_kernel_codification('2e874bcc-4cea-400f-803f-056c22c4a2bb', fixed_text).
narrative_ontology:cs_authority_grounding('2e874bcc-4cea-400f-803f-056c22c4a2bb', lineage).
narrative_ontology:cs_interpretation_layer_present('2e874bcc-4cea-400f-803f-056c22c4a2bb').
narrative_ontology:cs_reading_relation('2e874bcc-4cea-400f-803f-056c22c4a2bb', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('2e874bcc-4cea-400f-803f-056c22c4a2bb', nicene_creed_authority__liturgical_habituation_reading, forecloses).
narrative_ontology:cs_axiom('2e874bcc-4cea-400f-803f-056c22c4a2bb', foundational, creed_binds_metaphysical_assent).
narrative_ontology:cs_axiom_status(creed_binds_metaphysical_assent, holdable).
narrative_ontology:cs_axiom_grounding('2e874bcc-4cea-400f-803f-056c22c4a2bb', creed_binds_metaphysical_assent, theological).
narrative_ontology:cs_axiom('2e874bcc-4cea-400f-803f-056c22c4a2bb', foundational, deviation_warrants_sanction).
narrative_ontology:cs_axiom_status(deviation_warrants_sanction, holdable).
narrative_ontology:cs_axiom_grounding('2e874bcc-4cea-400f-803f-056c22c4a2bb', deviation_warrants_sanction, deontological).
narrative_ontology:cs_axiom('2e874bcc-4cea-400f-803f-056c22c4a2bb', secondary, civil_power_enforces_doctrinal_sanction).
narrative_ontology:cs_axiom_status(civil_power_enforces_doctrinal_sanction, overridden).
narrative_ontology:cs_axiom_grounding('2e874bcc-4cea-400f-803f-056c22c4a2bb', civil_power_enforces_doctrinal_sanction, conventional).
narrative_ontology:cs_reference_frame('2e874bcc-4cea-400f-803f-056c22c4a2bb', apostolic_conciliar_deposit).
narrative_ontology:cs_drift_state('2e874bcc-4cea-400f-803f-056c22c4a2bb', contemporary_ecumenical_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('2e874bcc-4cea-400f-803f-056c22c4a2bb', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, imperial_authorities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, dissenting_theologians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, parish_clergy).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, parish_clergy).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, homoousion_doctrine).
narrative_ontology:constraint_vindicates(nicene_creed_authority__strict_orthodox_reading, conciliar_authority_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the councils and doctrinal congregations that define what the creed requires, and administers the sanctions — excommunication, removal from teaching office, denial of communion — for deviation from the defined meaning. Collects the jurisdiction, deference, and (historically, with imperial and royal backing) property and civil enforcement that flow to the office which adjudicates the creed's meaning. Its exit is interpretive rather than physical: it can convene new councils, develop doctrine, and redefine what the creed binds without leaving its own structure.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).

% Convened and enforced the early councils' verdicts with civil power — exile, confiscation of property, and later capital statutes against heresy. Drew administrative cohesion from religious uniformity across a diverse empire. Withdrew enforcement as secular states separated from church jurisdiction; by the modern era it holds no enforcement role in the arrangement.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, imperial_authorities, beneficiary,
    powerful, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, imperial_authorities, agenda_setter).

% Teaches the creed as binding at the congregational level and administers its day-to-day discipline. Gains standing, livelihood, and local authority from being custodian of the one confession; simultaneously bears the discipline — bound by ordination vows and obedience, subject to the same sanctions for deviation, and without authority to reinterpret the creed independently.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, parish_clergy, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__strict_orthodox_reading, parish_clergy, payer).

% Are required to assent to the creed's metaphysical content as a condition of membership; private interpretation departing from the defined meaning counts as disobedience and costs communion. Their belonging — baptismal identity, sacramental life, family and community ties — is constituted inside the communion, so departure costs the identity itself rather than a membership fee.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, lay_interpreters, payer,
    powerless, biographical, identity_locked, continental).

% Hold trinitarian or christological positions the defined ontology condemns — Arian and subordinationist communities historically, dissenting communions since. Once condemned they lose office, property, and civil standing under the enforcement of the age, and persist as expelled, underground, or parallel communities. Their exit from the sanction is exit from the communion entirely; there is no licensed way to hold their position inside it.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, heterodox_communities, payer,
    powerless, generational, trapped, continental).

% Scholars and teachers whose work probes the creed's formulations and their history. Deviation costs faculty positions, teaching office, and publication approval; conformity costs the inquiry itself. Their exit is bounded by vocation: the tradition, libraries, and questions they are formed by exist almost entirely inside the communion that disciplines their conclusions.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, dissenting_theologians, payer,
    moderate, biographical, constrained, continental).

% Other communions — Orthodox, Protestant, Old Catholic and others — that recite the same creed while denying that any single hierarchy may bind its meaning or sanction their reading of it. They would contest the binding claim directly, but they stand outside the conversation because this reading's framework defines their position as the deviation it polices.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, ecumenical_churches, excluded,
    powerful, generational, mobile, global).

% Studies the creed's promulgation, enforcement, and evolution across the full interval from council records, canon law, and the literature of the condemned. Holds no seat in the dispute, bears no sanction, and collects no deference; documents the machinery's operation from outside every party's commitment.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__strict_orthodox_reading, historian_of_doctrine, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__strict_orthodox_reading, episcopal_hierarchy).
narrative_ontology:fixing_cost_class(nicene_creed_authority__strict_orthodox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one shared metaphysical confession across a dispersed, multi-generational, multi-lingual communion: a common rule for baptism and worship, mutual recognition between congregations, and a fixed boundary against theologies that dissolve the shared object of worship.
% TRANSFER_FUNCTION: Moves interpretive authority and disciplinary power from individual believers and local communities to the hierarchical center; moves assent upward as obedience owed to defined meaning; historically moved property, offices, and civil standing from the condemned to the church and its allied rulers.
% ABSENT_VOICES: The condemned had no seat: heterodox communities were defined and condemned by councils they did not attend, and their positions survive mainly as refuted summaries written by their opponents. Lay interpreters and dissenting theologians sit outside the deliberative bodies. Other churches that recite the creed while denying the binding claim are absent by construction — the reading defines them as the deviation it polices.
% DISAPPEARANCE_RATIONALE: If the binding-with-sanction vanished overnight, canon law's heresy machinery, the jurisdictional boundaries it polices, and the hierarchy's adjudicative role would dissolve; communities would reorganize around voluntary assent — the mode in which the sibling readings already operate. The creed text would persist and still be recited, but as liturgy and witness rather than as enforced ontology, and the condemned class would cease to exist as a category.
% FOUNDING_PROBLEM: The fourth-century crisis over Christ's relation to the Father (the Arian controversy) and the need of a dispersed imperial church for one rule of faith for baptism and worship against subordinationist and gnostic alternatives.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of doctrine (from Harnack and Kelly to contemporary patristics) corroborate from outside the benefiting parties that the Arian crisis was real and that the creed resolved it. Ecumenical dialogues and the sibling communions attest that the binding-with-sanction form now persists primarily as jurisdictional discipline rather than as response to a live metaphysical emergency. Heterodox-descended communities attest the sanction function from the receiving end. No party outside the hierarchy attests that the sanction machinery itself remains necessary.
narrative_ontology:disappearance_verdict(nicene_creed_authority__strict_orthodox_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__strict_orthodox_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__strict_orthodox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__strict_orthodox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__strict_orthodox_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.60 (end-state): substantial, but reading-indexed — in the strict reading's own lights much of the transfer (assent owed, discipline administered) is warranted guardianship, so it is not rated at the 0.8+ an external critic of the same referent would author. Suppression is 0.50 at end-state: canonical sanctions (excommunication, removal from teaching office, denial of communion) remain real, but the coercive-lethal apparatus is gone; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change — the machinery's build-up (0.35 at Nicaea to 0.82 at the Reformation-era peak) and subsequent decay (0.50 today). Theater ratio is low-to-moderate (0.32): most enforcement remains functional, but a rising share of censures are formal acts with no operative effect on belief or practice. Accessibility collapse is 0.48: within the communion, alternatives collapse (the defined meaning forecloses private metaphysics), but across the landscape heterodox and sibling communities persist, so alternatives are suppressed rather than eliminated. Resistance is 0.58: schisms, the Reformation, modern dissent, and academic historical criticism have contested the binding claim continuously. All series run on one shared ten-point grid (325–2025) so every tracked metric is authored at every examined time point. Coordination type is identity_coordination: the text's primary function is membership boundary maintenance. Alert per the identity-coordination gaming risk: the enforcement coupling concentrates on the powerless (lay interpreters, condemned communities) at continental scope while the beneficiaries sit at the institutional level — the complexity offset accommodates genuine boundary-maintenance work; it does not launder that asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the episcopal seat the arrangement is guardianship: a unity the hierarchy maintains at real cost to itself, with sanction as the necessary edge of care. From the payer seats the same structure operates as enforced transfer — interpretive authority and, historically, property and civil standing moved from the condemned and the constrained to the center. The imperial seat (historical) computed the arrangement as administrative convenience. The engine derives these divergent per-seat classifications from power, exit, and declared position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The episcopal hierarchy sits near the beneficiary end (d≈0.05): it defines the binding content and collects the jurisdiction. Imperial authorities sit near-symmetric-beneficiary (d≈0.25): indirect benefit through cohesion, with mobile exit once enforcement was withdrawn. Parish clergy are genuinely dual-positioned: derivation from their beneficiary role alone would place them near d≈0.15–0.20, but they also bear the discipline — bound by vow, sanctionable, without reinterpretive authority — so a directionality override sets the organized power atom to 0.35. Dissenting theologians (d≈0.80) and lay interpreters (d≈0.85, identity-locked: departure costs the identity itself) sit near the target end. Heterodox communities sit nearest full target (d≈0.95): they are the sanction's object. The single override is justified because the derivation cannot see dual positioning from role alone; every other seat derives correctly from declared position plus exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested. The specific fourth-century emergency (Arian subordinationism) is historically resolved — corroborated by secular historians outside the benefiting parties — and the enforcement warrant that belonged to that era (imperial coercion of conciliar verdicts) is not merely dead but formally repudiated within the tradition itself: the overridden axiom civil_power_enforces_doctrinal_sanction, and the acknowledged authority erosion in the drift state. What persists is the unity function, live in altered form, carried now by jurisdictional discipline rather than coercion. The tangled_rope classification prevents mislabeling in both directions: a pure-snare reading would erase the genuine coordination that the sibling readings demonstrate is possible without sanction machinery; a pure-rope reading would erase the condemned class the sanction machinery actually produces. The temporal series shows the constraint decaying toward a lower-extraction steady state rather than toward pure theater — theater_ratio rises but stays below 0.35, and the extraction decline tracks the enforcement decay rather than lagging it — so the mandate is partially live with a dead coercive warrant, not a zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading (strict_orthodox_reading) of the kernel nicene_creed_authority; how would the classification change if a sibling reading — symbolic_confessional_reading or liturgical_habituation_reading — were the operative constraint instead?',
    'Classify the sibling stories separately over the same referent: the symbolic reading authors low epsilon (no binding, no sanction machinery, authority from discernment) and the habituation reading authors low-to-moderate epsilon with a different beneficiary structure (liturgical formation specialists) and no heresy-policing victims; compare per-seat classifications across the family.',
    'If a sibling reading governed, the victim set (heterodox communities, lay interpreters) empties, the sanction machinery drops out, epsilon falls toward the coordination floor, and the classification moves from tangled_rope toward rope; the high-epsilon profile here is reading-indexed, not a property of the text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    disagreement_location_assent_mode,
    'Where exactly do the three readings of the kernel disagree? The disagreement is located in the mode of assent the creed demands: cognitive-metaphysical assent backed by sanction (this reading), performative participation without cognitive requirement (liturgical habituation), or historical witness received by personal faith (symbolic confessional).',
    'No data resolves it: the readings are different constraints authored from different committer stances over one text. Resolution would be a decision about which reading governs — an exercise of authority or preference, not a discovery.',
    'Whichever reading governs determines whether heresy policing exists at all: only this reading entails sanction machinery, so the kernel''s classification is decided by reading selection rather than by measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_assent_mode, conceptual, 'The structural element the sibling readings contest is the demanded mode of assent, not the text itself.').

omega_variable(
    sanction_separability,
    'Is coercive sanction structurally necessary to the creed''s coordination function, or separable from it?',
    'Compare communions that recite the creed with no heresy courts against communions that retain doctrinal sanction: if unity and boundary maintenance hold without sanction machinery, the sanction is separable and rides on the coordination rather than enabling it.',
    'If separable, the tangled_rope reading overstates the coordination share and the constraint trends snare; if inseparable, part of the measured suppression is the price of the unity this reading exists to protect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanction_separability, empirical, 'Whether the sanction machinery is structurally coupled to the creed''s unity function.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of lay interpreters structural (sanction machinery, denial of teaching office, exclusion from deliberation) or internalized (a formed conscience that experiences private judgment as disobedience)?',
    'Post-exit trajectory: track believers who leave the strict communion — if the capacity for independent interpretation remains impaired after exit (guilt structures, inability to read the tradition critically), the suppression is partly internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure — the constraint travels with the believer after exit — and the identity-locked exit classification of lay_interpreters is reinforced rather than merely declared.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for the lay seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 325, 0.08).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t381, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 381, 0.1).
narrative_ontology:measurement_basis(nice_tr_t381, observed).
narrative_ontology:measurement(nice_tr_t553, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 553, 0.12).
narrative_ontology:measurement_basis(nice_tr_t553, observed).
narrative_ontology:measurement(nice_tr_t787, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 787, 0.14).
narrative_ontology:measurement_basis(nice_tr_t787, observed).
narrative_ontology:measurement(nice_tr_t1054, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1054, 0.16).
narrative_ontology:measurement_basis(nice_tr_t1054, observed).
narrative_ontology:measurement(nice_tr_t1231, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1231, 0.18).
narrative_ontology:measurement_basis(nice_tr_t1231, observed).
narrative_ontology:measurement(nice_tr_t1546, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1546, 0.22).
narrative_ontology:measurement_basis(nice_tr_t1546, observed).
narrative_ontology:measurement(nice_tr_t1870, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1870, 0.28).
narrative_ontology:measurement_basis(nice_tr_t1870, observed).
narrative_ontology:measurement(nice_tr_t1965, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement_basis(nice_tr_t1965, observed).
narrative_ontology:measurement(nice_tr_t2025, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(nice_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 325, 0.42).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t381, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 381, 0.52).
narrative_ontology:measurement_basis(nice_be_t381, observed).
narrative_ontology:measurement(nice_be_t553, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 553, 0.57).
narrative_ontology:measurement_basis(nice_be_t553, observed).
narrative_ontology:measurement(nice_be_t787, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 787, 0.55).
narrative_ontology:measurement_basis(nice_be_t787, observed).
narrative_ontology:measurement(nice_be_t1054, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1054, 0.61).
narrative_ontology:measurement_basis(nice_be_t1054, observed).
narrative_ontology:measurement(nice_be_t1231, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1231, 0.7).
narrative_ontology:measurement_basis(nice_be_t1231, observed).
narrative_ontology:measurement(nice_be_t1546, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1546, 0.74).
narrative_ontology:measurement_basis(nice_be_t1546, observed).
narrative_ontology:measurement(nice_be_t1870, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1870, 0.68).
narrative_ontology:measurement_basis(nice_be_t1870, observed).
narrative_ontology:measurement(nice_be_t1965, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 1965, 0.62).
narrative_ontology:measurement_basis(nice_be_t1965, observed).
narrative_ontology:measurement(nice_be_t2025, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(nice_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t325, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 325, 0.35).
narrative_ontology:measurement_basis(nice_su_t325, observed).
narrative_ontology:measurement(nice_su_t381, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 381, 0.5).
narrative_ontology:measurement_basis(nice_su_t381, observed).
narrative_ontology:measurement(nice_su_t553, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 553, 0.56).
narrative_ontology:measurement_basis(nice_su_t553, observed).
narrative_ontology:measurement(nice_su_t787, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 787, 0.52).
narrative_ontology:measurement_basis(nice_su_t787, observed).
narrative_ontology:measurement(nice_su_t1054, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1054, 0.6).
narrative_ontology:measurement_basis(nice_su_t1054, observed).
narrative_ontology:measurement(nice_su_t1231, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1231, 0.78).
narrative_ontology:measurement_basis(nice_su_t1231, observed).
narrative_ontology:measurement(nice_su_t1546, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1546, 0.82).
narrative_ontology:measurement_basis(nice_su_t1546, observed).
narrative_ontology:measurement(nice_su_t1870, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1870, 0.72).
narrative_ontology:measurement_basis(nice_su_t1870, observed).
narrative_ontology:measurement(nice_su_t1965, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement_basis(nice_su_t1965, observed).
narrative_ontology:measurement(nice_su_t2025, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(nice_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% One colloquial object — 'the authority of the Nicene Creed' — decomposes into three structurally distinct constraints: this strict-orthodox reading, symbolic_confessional_reading, and liturgical_habituation_reading. Each authors its own epsilon over the same standing referent (the creed's operative authority in the communions that recite it): this reading authors substantial epsilon because its premise entails sanction machinery and a condemned class; the siblings author low epsilon because they entail no binding-with-sanction. The values differ because the readings differ — reading-indexed epsilon over a fixed referent — not because the text is measured differently. The text's promulgation is the upstream, high-confidence claim each reading cites as warrant; the family links make that shared-citation structure visible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__strict_orthodox_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
