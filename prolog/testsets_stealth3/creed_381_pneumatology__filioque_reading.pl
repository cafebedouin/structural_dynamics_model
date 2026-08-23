% ============================================================================
% CONSTRAINT STORY: creed_381_pneumatology__filioque_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Filioque Clause and Magisterial Clarification Authority (filioque_reading)
 *   domain: historical_theology/ecclesiastical_authority/commitment_systems
 *
 * SUMMARY:
 *   The Western church recites the Creed of 381 with the Filioque added ('and
 *   the Son' appended to the Spirit's procession) and holds that the papal
 *   and conciliar magisterium possesses authority to render explicit what the
 *   received text contains implicitly, binding the whole communion. The
 *   clause entered general Latin use through Carolingian chapel practice, was
 *   adopted into the Roman Mass in 1014, was defined as required belief at
 *   Florence in 1439, and its defense drove the medieval confrontations with
 *   the Eastern churches, which never consented to the addition and whose
 *   assent was demanded under anathema. The arrangement coordinates a single
 *   Trinitarian confession across the Latin communion while transferring
 *   definitional authority from conciliar consent structures to the Roman
 *   see; the Eastern churches bear the assent demand, the anathema risk, and,
 *   after 1204, installed Latin hierarchs. This file authors the
 *   filioque_reading only, as a clean epsilon-invariant constraint; the
 *   kernel contest is routed to the omega variables and kernel_context.
 *
 * KEY AGENTS:
 *   - roman_apostolic_see: Agenda-setter and primary collector (institutional/arbitrage) - inserts, defines, and enforces; collects obedience, jurisdiction, and precedent
 *   - carolingian_imperial_court: Secondary beneficiary (powerful/mobile) - sponsored the interpolated creed for anti-Arian and anti-Byzantine ends without running enforcement
 *   - western_latin_episcopate: Dual-positioned beneficiary/payer (organized/constrained) - gains uniform confession, loses synodal autonomy
 *   - eastern_patriarchates: Primary bearer of costs (organized/trapped) - absorb the assent demand and anathema risk with no pre-consent venue
 *   - constantinopolitan_patriarchate: Principal resisting bearer of costs (organized/trapped) - guardian of the unamended 381 text
 *   - athonite_monastic_opposition: Identity-locked bearer of costs (moderate/identity_locked) - fidelity to the received symbol constitutes vocation
 *   - byzantine_imperial_government: Coerced intermediary (powerful/constrained) - pressed union through for promised aid that never arrived
 *   - eastern_bishops_absent_from_florence: Excluded voice (organized/constrained) - absent from the defining sessions whose absence enabled the definition
 *   - ecumenical_dialogue_commissions: Analytical observer (institutional/analytical) - documents the structure without administering it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(creed_381_pneumatology__filioque_reading, 0.7).
domain_priors:suppression_score(creed_381_pneumatology__filioque_reading, 0.52).
domain_priors:theater_ratio(creed_381_pneumatology__filioque_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(creed_381_pneumatology__filioque_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(creed_381_pneumatology__filioque_reading, tangled_rope).
narrative_ontology:human_readable(creed_381_pneumatology__filioque_reading, "Filioque Clause and Magisterial Clarification Authority (filioque_reading)").
narrative_ontology:topic_domain(creed_381_pneumatology__filioque_reading, "historical_theology/ecclesiastical_authority/commitment_systems").

domain_priors:requires_active_enforcement(creed_381_pneumatology__filioque_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(creed_381_pneumatology__filioque_reading, '346960b7-2272-41a1-8060-0cefa7d199ce').
narrative_ontology:cs_kernel_codification('346960b7-2272-41a1-8060-0cefa7d199ce', fixed_text).
narrative_ontology:cs_authority_grounding('346960b7-2272-41a1-8060-0cefa7d199ce', lineage).
narrative_ontology:cs_interpretation_layer_present('346960b7-2272-41a1-8060-0cefa7d199ce').
narrative_ontology:cs_reading_relation('346960b7-2272-41a1-8060-0cefa7d199ce', creed_381_pneumatology__monoprocession_reading, forecloses).
narrative_ontology:cs_reading_relation('346960b7-2272-41a1-8060-0cefa7d199ce', creed_381_pneumatology__ecumenical_reunion_reading, forecloses).
narrative_ontology:cs_axiom('346960b7-2272-41a1-8060-0cefa7d199ce', foundational, spirit_proceeds_from_father_and_son).
narrative_ontology:cs_axiom_status(spirit_proceeds_from_father_and_son, holdable).
narrative_ontology:cs_axiom_grounding('346960b7-2272-41a1-8060-0cefa7d199ce', spirit_proceeds_from_father_and_son, theological).
narrative_ontology:cs_axiom('346960b7-2272-41a1-8060-0cefa7d199ce', foundational, magisterium_may_render_implicit_explicit).
narrative_ontology:cs_axiom_status(magisterium_may_render_implicit_explicit, holdable).
narrative_ontology:cs_axiom_grounding('346960b7-2272-41a1-8060-0cefa7d199ce', magisterium_may_render_implicit_explicit, conventional).
narrative_ontology:cs_reference_frame('346960b7-2272-41a1-8060-0cefa7d199ce', implicit_double_procession_contained_in_381_symbol).
narrative_ontology:cs_drift_state('346960b7-2272-41a1-8060-0cefa7d199ce', contemporary_ecumenical_dialogue_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('346960b7-2272-41a1-8060-0cefa7d199ce', '').
narrative_ontology:cs_kernel_id(creed_381_pneumatology__filioque_reading, creed_381_pneumatology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, roman_apostolic_see).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, carolingian_imperial_court).
narrative_ontology:constraint_beneficiary(creed_381_pneumatology__filioque_reading, western_latin_episcopate).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, eastern_patriarchates).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, constantinopolitan_patriarchate).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, athonite_monastic_opposition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, western_latin_episcopate).
narrative_ontology:constraint_victim(creed_381_pneumatology__filioque_reading, byzantine_imperial_government).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, papal_plenitudo_potestatis_doctrine).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, implicit_containment_hermeneutic).
narrative_ontology:constraint_vindicates(creed_381_pneumatology__filioque_reading, double_procession_theology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Western creedal recension containing the Filioque: adopted the clause into the Roman Mass in 1014, defined the double procession as required belief at Florence in 1439, and disciplines churches whose confession omits it. Collects the obedience of the Latin communion and the precedent value of every successful clarification, which strengthens its claim to render explicit what the received texts leave implicit. Retracting the arrangement would mean unwinding definitions issued under its own authority, a step no occupant of the see has taken.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, roman_apostolic_see, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, roman_apostolic_see, beneficiary).

% Sponsored the interpolated creed in its chapel and missals from the 790s onward, pressing it against surviving Arian circles at home and Byzantine theological influence abroad. Collected the political and theological differentiation benefit without operating the enforcement machinery; when the clause became diplomatically costly, later courts simply stopped pressing it.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, carolingian_imperial_court, beneficiary,
    powerful, generational, mobile, continental).

% Recites and teaches the clarified creed, gaining a uniform confessional standard across diverse provinces. Pays for the uniformity in narrowed provincial autonomy: doctrinal initiative migrates upward to the see, and provincial synods lose the standing to vary or develop the text locally.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, western_latin_episcopate, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(creed_381_pneumatology__filioque_reading, western_latin_episcopate, payer).

% Alexandria, Antioch, and Jerusalem receive the demand to confess the added clause as a condition of communion with the West, backed by anathema and, after 1204, by Latin hierarchs installed over their sees. Remaining in communion means absorbing the interpolation; withholding assent means bearing the anathema and the rupture. Their conciliar machinery gives them collective weight but no venue where their consent is sought before the fact.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_patriarchates, payer,
    organized, generational, trapped, continental).

% Guards the 381 text as received and insists that no word be added without an ecumenical council in which it participates. Bears the direct confrontations: the Photian contestation, the 1054 rupture, the crusader occupation, and the Florentine definition aimed at its submission. Its way out runs through schism, which it ultimately accepted at catastrophic cost rather than confess the clause.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, constantinopolitan_patriarchate, payer,
    organized, generational, trapped, regional).

% Monastic communities on Athos, and figures such as Mark of Ephesus at Florence, anchor resistance to the added clause in fidelity to the received symbol. Their vocation is constituted by that fidelity; abandoning it would dissolve the identity that organizes their entire life, so stepping back from the dispute is not a live option for them. They supply the theological argumentation and the witness of refusal.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, athonite_monastic_opposition, payer,
    moderate, civilizational, identity_locked, local).

% At Florence pressed the church delegation toward assent because the empire needed Western military aid against the Ottomans; the promised help never materially arrived. The government administered pressure it also suffered and gained little; after 1443 the union was repudiated in Constantinople itself.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, byzantine_imperial_government, payer,
    powerful, immediate, constrained, regional).

% The overwhelming majority of Eastern bishops never reached the Ferrara-Florence council; the defining sessions were carried by a small delegation under imperial pressure. They would have objected to a definition issued without their presence; their absence is what allowed the definition to proceed, and their later rejection is what voided its reception.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, eastern_bishops_absent_from_florence, excluded,
    organized, generational, constrained, regional).

% Joint commissions and study groups, including the body behind the 1995 Vatican clarification document, examine the two traditions' formulations without administering anything. They document where the formulas converge and where the authority question blocks convergence; their findings bind no one.
narrative_ontology:constraint_stakeholder(creed_381_pneumatology__filioque_reading, ecumenical_dialogue_commissions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(creed_381_pneumatology__filioque_reading, roman_apostolic_see).
narrative_ontology:fixing_cost_class(creed_381_pneumatology__filioque_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a single Trinitarian confession across geographically dispersed churches with divergent theological schools: one fixed formula, one authorized meaning, enforced uniformly, with an explicit anti-subordinationist content at a time when Arian kingdoms ruled much of the former Western empire.
% TRANSFER_FUNCTION: Moves doctrinal assent and liturgical conformity from all churches in communion toward the Roman see; moves from the Eastern churches the authority to say what the creed means, along with (after 1204) jurisdictional control of their sees; places anathema and rupture-risk onto whoever declines the clarified formula.
% ABSENT_VOICES: The Eastern bishops absent from the Florentine defining sessions would have objected to a definition carried by a coerced delegation; the 381 fathers' successors in the Eastern sees object continuously and are heard only after the fact; Eastern monastic witnesses were recorded and not heeded. The unanimity of the definition is an artifact of who was in the room.
% DISAPPEARANCE_RATIONALE: If the clause were withdrawn and the clarification-authority claim retracted overnight, the Latin communion would need a new mechanism for settling creedal meaning (conciliar consent or bilateral recognition), the see would lose a load-bearing precedent for unilateral definition later generalized at Vatican I, and the principal East-West obstacle would vanish, immediately rearranging communion negotiations.
% FOUNDING_PROBLEM: The creed of 381 confessed the Spirit as proceeding from the Father without specifying the Son's role; Western theology developed the double-procession doctrine; Arian and subordinationist teaching among the Germanic kingdoms made an explicit anti-Arian confession pastorally urgent; and the West needed an answer to how a fixed symbol may legitimately develop - who may clarify it?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Carolingian capitularies and court writings attest the anti-Arian motive contemporaneously; Photius's encyclical to the Eastern patriarchs (867) and the acts of the Council of 879-880 attest the Eastern account of an unconsented addition; the Vatican's own 1995 clarification concedes that the Greek mono-patrist formulation expresses the same faith, an admission from within the benefiting institution that the binding character of the clarification is disputed. No source outside the dispute attests that the clarification authority itself remains live; that status is asserted by the see and denied by the Orthodox churches.
narrative_ontology:disappearance_verdict(creed_381_pneumatology__filioque_reading, world_rearranges).
narrative_ontology:founding_problem_status(creed_381_pneumatology__filioque_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(creed_381_pneumatology__filioque_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(creed_381_pneumatology__filioque_reading, 'none', 1).
narrative_ontology:epsilon_provenance(creed_381_pneumatology__filioque_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.70 for the standing arrangement - the interpolated creed plus the clarification-authority claim - because it transfers definitional power from conciliar consent to a single see and demands assent under anathema; the referent is the imposed regime itself, assessed even by this reading's own lights, since the arrangement's own theory holds that clarification serves communion and the enforcement record (anathemas, installed Latin hierarchs, coerced Florentine assent) violates that theory procedurally regardless of the doctrine's truth. Suppression (0.52) is a raw structural property, unscaled by power or scope: it reflects the residual coercive apparatus (non-assent bars full communion) after the medieval enforcement peak decayed. Theater_ratio (0.38) is moderate: the theological and liturgical substance is real throughout, but Florence's coerced signatures, the paper-only union, and pro-forma consultation inflate the performative share. Accessibility_collapse (0.62) is jurisdictionally bounded: within the enforcing communion alternatives collapsed completely (denial equals heresy), but the Eastern churches retained and exercised the alternative, so collapse falls far short of total. Resistance (0.80) is near the top of the range: the Photian contestation, the 1054 rupture, Athonite and Mark-of-Ephesus opposition, the post-Florence repudiations, and continuing non-assent. Fixing is prohibitive for the only seat that could fix it: withdrawing the clause would unravel the precedent structure on which the see's later definitional authority (culminating in 1870) rests, a cost vastly exceeding any benefit to the see. Claim and metrics are independent authored facts: claimed_type tangled_rope is my structural judgment (genuine anti-Arian and liturgical coordination function plus asymmetric override of Eastern consent, actively enforced); the engine computes per-seat classifications from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is legitimate development the see is charged to guard; from the payer seats the same structure operates as an override of conciliar consent. The divergence is driven by directionality and exit: the see holds arbitrage-grade control (it can amend, define, or retract), while Constantinople and the Athonite communities sit trapped or identity_locked, so identical canonical facts yield opposite experienced arrangements. The western episcopate straddles the gap - uniform confession is a real benefit, lost synodal autonomy a real cost - which is why its seat should compute intermediate. The identity-lock mechanism is vocational-religious: the monks' self-concept is constituted by fidelity to the received symbol; if that frame broke (as it partially has for communities that accepted union), their seat would migrate toward constrained and their effective burden would fall.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection: the see collects obedience, jurisdiction, and precedent (d near the beneficiary end); the Carolingian court collected political-theological differentiation without administering enforcement; the Latin episcopate collects liturgical uniformity while paying autonomy (mid-range d). Victim declarations map to borne costs: the Eastern patriarchates bear the assent demand and anathema risk with no pre-consent venue (d near the target end, amplified by trapped exit); Constantinople bears the direct confrontation; the monastic opposition bears the full weight because identity-lock removes even internal exit. The Byzantine government is a coerced intermediary - it administered pressure it also suffered, with an immediate-horizon survival motive pulling it toward compliance. The excluded Eastern bishops are the enforcement object's negative image: the definition proceeded because they were absent. Continental spatial scope makes verification of genuine consent hardest, which the engine folds into effective extraction for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding anti-Arian mandate died with Arianism's absorption, but the arrangement acquired a second function - vindicating magisterial clarification as precedent - that keeps it live for the agenda-setter; hence founding_problem_status is contested rather than dead, and no zombie flag is warranted. The tangled_rope claim prevents both mislabelings: a pure-extraction reading would erase the sincere pneumatological content and the real liturgical coordination the clause performed for a fragmented West; a pure-coordination reading would erase the asymmetric override of Eastern consent that the same structure enforced. Decomposition test: sever the clarification-authority claim from the clause and the residue trends toward rope; keep the claim and abandon the clause and the precedent machinery persists as pure authority consolidation - which is why the sibling readings are authored as separate constraints rather than folded into this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (filioque_reading) of the kernel creed_381_pneumatology; what structurally changes under the sibling readings monoprocession_reading and ecumenical_reunion_reading?',
    'Compare the three stories'' beneficiary/victim sets and epsilon values: the monoprocession reading relocates the cost-bearing seat to the Western interpolators and the collecting seat to conciliar consent; the reunion reading dissolves the asymmetry into bilateral recognition. The disagreement is located in the authority-to-amend element (who may render the creed''s implicit content explicit), secondarily in the procession content itself.',
    'Classification is reading-relative: the same kernel yields a high-epsilon enforced-development arrangement under this reading and a consent-inviability arrangement under the monoprocession reading; cross-reading comparison is valid only at the kernel level, never by merging epsilon across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame record: this story instantiates the filioque_reading of kernel creed_381_pneumatology; sibling readings alter the victim/beneficiary topology.').

omega_variable(
    content_truth_vs_procedural_extraction,
    'Does authoritative clarification count as extraction when the clarified content is true - is epsilon governed by the truth of the double procession or by the consent procedures overridden?',
    'Separate the registers: assess the arrangement''s consent structure (who was asked, who assented, under what pressure) independently of the doctrine''s truth-value; only a framework that lets content-truth license unilateral imposition would lower epsilon.',
    'If content-truth licenses imposition, epsilon falls toward coordination territory; if procedure governs, epsilon stays high regardless of the doctrine''s truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_truth_vs_procedural_extraction, conceptual, 'Whether doctrinal truth can offset procedural override in the epsilon assessment.').

omega_variable(
    florence_assent_authenticity,
    'Did the Florentine assent constitute the consent the arrangement''s own theory of authority requires?',
    'Examine the acta: delegation composition, imperial pressure, the recorded dissent of Mark of Ephesus, and the post-1443 repudiations by Constantinople, Alexandria, Antioch, and Jerusalem.',
    'Coerced or unrepresentative assent means the definition never achieved reception, supporting high epsilon and the contested founding-problem status; genuine representative assent would support the clarification framing and lower the extraction assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(florence_assent_authenticity, empirical, 'Authenticity of the Florentine consent on which the binding claim rests.').

omega_variable(
    softening_vs_enforcement_decay,
    'Is the post-conciliar decline in suppression a decay of enforcement capacity (the arrangement weakening) or a strategic softening that preserves the binding claim while lowering its cost (performative share rising)?',
    'Track whether any post-1965 magisterial instrument formally downgrades the Florentine definition''s binding status; if none does while dialogue proceeds, the softening is strategic rather than decaying.',
    'Capacity decay points toward a transitional-support or inertial trajectory; strategic softening keeps the arrangement a live enforced hybrid with rising theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(softening_vs_enforcement_decay, empirical, 'Whether contemporary softening reflects enforcement decay or preserved-binding strategy.').

omega_variable(
    implicit_containment_framing_underdetermination,
    'Is the papal magisterium the right framing of this constraint''s authority, or is the load-bearing element the implicit-containment hermeneutic - the claim that the 381 symbol already contained the Filioque implicitly - without which the amendment reads as naked alteration?',
    'Test whether the arrangement survives removal of the hermeneutic: if the clarification claim depends on the hermeneutic for legitimacy, the hermeneutic is the kernel-stabilizer and the framing should shift to it.',
    'Under the hermeneutic framing, kernel codification shifts toward distributed (the hermeneutic is contested across traditions) and the authority seat weakens, changing the computed commitment-system pattern; under the magisterium framing the pattern is anchored-lineage as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_containment_framing_underdetermination, conceptual, 'CS-framing under-determination: magisterium-as-authority versus hermeneutic-as-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(creed_381_pneumatology__filioque_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cree_tr_t0, creed_381_pneumatology__filioque_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cree_tr_t0, observed).
narrative_ontology:measurement(cree_tr_t6, creed_381_pneumatology__filioque_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(cree_tr_t6, observed).
narrative_ontology:measurement(cree_tr_t10, creed_381_pneumatology__filioque_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(cree_tr_t10, observed).
narrative_ontology:measurement(cree_tr_t14, creed_381_pneumatology__filioque_reading, theater_ratio, 14, 0.44).
narrative_ontology:measurement_basis(cree_tr_t14, observed).
narrative_ontology:measurement(cree_tr_t20, creed_381_pneumatology__filioque_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement_basis(cree_tr_t20, observed).
narrative_ontology:measurement(cree_tr_t24, creed_381_pneumatology__filioque_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(cree_tr_t24, observed).
narrative_ontology:measurement(cree_tr_t27, creed_381_pneumatology__filioque_reading, theater_ratio, 27, 0.33).
narrative_ontology:measurement_basis(cree_tr_t27, observed).
narrative_ontology:measurement(cree_tr_t30, creed_381_pneumatology__filioque_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(cree_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cree_be_t0, creed_381_pneumatology__filioque_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cree_be_t0, observed).
narrative_ontology:measurement(cree_be_t6, creed_381_pneumatology__filioque_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement_basis(cree_be_t6, observed).
narrative_ontology:measurement(cree_be_t10, creed_381_pneumatology__filioque_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement_basis(cree_be_t10, observed).
narrative_ontology:measurement(cree_be_t14, creed_381_pneumatology__filioque_reading, base_extractiveness, 14, 0.72).
narrative_ontology:measurement_basis(cree_be_t14, observed).
narrative_ontology:measurement(cree_be_t20, creed_381_pneumatology__filioque_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement_basis(cree_be_t20, observed).
narrative_ontology:measurement(cree_be_t24, creed_381_pneumatology__filioque_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(cree_be_t24, observed).
narrative_ontology:measurement(cree_be_t27, creed_381_pneumatology__filioque_reading, base_extractiveness, 27, 0.73).
narrative_ontology:measurement_basis(cree_be_t27, observed).
narrative_ontology:measurement(cree_be_t30, creed_381_pneumatology__filioque_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement_basis(cree_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cree_su_t0, creed_381_pneumatology__filioque_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(cree_su_t0, observed).
narrative_ontology:measurement(cree_su_t6, creed_381_pneumatology__filioque_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(cree_su_t6, observed).
narrative_ontology:measurement(cree_su_t10, creed_381_pneumatology__filioque_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(cree_su_t10, observed).
narrative_ontology:measurement(cree_su_t14, creed_381_pneumatology__filioque_reading, suppression_requirement, 14, 0.75).
narrative_ontology:measurement_basis(cree_su_t14, observed).
narrative_ontology:measurement(cree_su_t20, creed_381_pneumatology__filioque_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement_basis(cree_su_t20, observed).
narrative_ontology:measurement(cree_su_t24, creed_381_pneumatology__filioque_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(cree_su_t24, observed).
narrative_ontology:measurement(cree_su_t27, creed_381_pneumatology__filioque_reading, suppression_requirement, 27, 0.56).
narrative_ontology:measurement_basis(cree_su_t27, observed).
narrative_ontology:measurement(cree_su_t30, creed_381_pneumatology__filioque_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(cree_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(creed_381_pneumatology__filioque_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__monoprocession_reading).
narrative_ontology:affects_constraint(creed_381_pneumatology__filioque_reading, creed_381_pneumatology__ecumenical_reunion_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the Filioque controversy' decomposes into three structurally distinct constraints - three readings of one kernel (creed_381_pneumatology). This story authors the filioque_reading: the enforced Latin development arrangement, with the papal see as collector and the Eastern churches as cost-bearers, high epsilon. The monoprocession_reading authors the inverse arrangement (the 381 text's inviolability, with unilateral amendment as the breach); the ecumenical_reunion_reading authors the bilateral-recognition arrangement (regional diversity within one communion). Each has its own epsilon, beneficiaries, and victims; the upstream dogmatic definition (this story) structurally influenced the downstream reunion negotiations (Lyon II, Florence, Brest), which is why the family edges run from this reading to the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
