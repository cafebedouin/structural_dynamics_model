% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage Indissolubility (Hierarchical Ontological Reading)
 *   domain: religious/doctrinal
 *
 * SUMMARY:
 *   The Roman Catholic Church's doctrine of sacramental marriage
 *   indissolubility, read through a hierarchical ontological lens: marriage
 *   is not a relationship dependent on the parties' continued consent or
 *   flourishing, but an objective sacramental reality that persists as a fact
 *   of church law and divine order. This reading instantiates the constraint
 *   as one way to interpret the contested kernel 'marriage_sacrament'. The
 *   magisterium defends it as apostolic tradition; the sibling civic_pastoral
 *   reading treats it as doctrinal, but emphasizes pastoral mercy for those
 *   whose marriages have fractured beyond repair. The extractive structure
 *   comes from exclusion: divorced Catholics seeking remarriage must petition
 *   the institutional tribunal for an annulment (a declaration that the
 *   sacramental bond was never validly formed). The process is costly,
 *   lengthy, uncertain, and imposes a burden of proof on the petitioner to
 *   demonstrate defect in the original consent. Those unable to obtain
 *   annulment and who remarry civilly are barred from Eucharist and full
 *   sacramental participation. The constraint is claimed as rope (real
 *   coordination of marriage understanding) but operates with the suppression
 *   and enforcement characteristics of tangled_rope: genuine coordination
 *   function + asymmetric extraction via institutional gatekeeping.
 *
 * KEY AGENTS:
 *   - magisterium (institutional agenda_setter, powerful — defines and maintains the doctrine)
 *   - canonical_tribunal_apparatus (institutional agenda_setter, powerful — adjudicates annulment petitions)
 *   - divorced_catholics_seeking_remarriage (powerless payers, identity_locked — bear exclusion costs without control over adjudication)
 *   - remarried_catholics_without_annulment (powerless payers, identity_locked — structurally excluded from sacramental participation)
 *   - first_spouses_unreconciable_to_remarriage (excluded from proceedings but have relational stake in the outcome)
 *   - pastoral_clergy_supporting_remarriage (excluded from doctrine-making but encounter the human cost; cognitive dissonance between doctrine and pastoral judgment)
 *   - vatican_magisterium_council (observer/analytical seat; authority to revise doctrine but rarely exercises it)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.68).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.72).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage Indissolubility (Hierarchical Ontological Reading)").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious/doctrinal").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, '5e0f050f-bd6d-44fe-8b92-5f0c26e46913').
narrative_ontology:cs_kernel_codification('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', fixed_text).
narrative_ontology:cs_authority_grounding('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', extraction).
narrative_ontology:cs_interpretation_layer_present('5e0f050f-bd6d-44fe-8b92-5f0c26e46913').
narrative_ontology:cs_reading_relation('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', foundational, marriage_bond_ontologically_indissoluble).
narrative_ontology:cs_axiom_status(marriage_bond_ontologically_indissoluble, holdable).
narrative_ontology:cs_axiom_grounding('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', marriage_bond_ontologically_indissoluble, theological).
narrative_ontology:cs_axiom('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', foundational, institutional_hierarchy_adjudicates_sacramental_status).
narrative_ontology:cs_axiom_status(institutional_hierarchy_adjudicates_sacramental_status, holdable).
narrative_ontology:cs_axiom_grounding('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', institutional_hierarchy_adjudicates_sacramental_status, conventional).
narrative_ontology:cs_reference_frame('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', apostolic_tradition_sacramental_indissolubility).
narrative_ontology:cs_drift_state('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', post_vatican_ii_pastoral_reform, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5e0f050f-bd6d-44fe-8b92-5f0c26e46913', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, magisterium).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunal_apparatus).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, remarried_catholics_without_annulment).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_marriage_ontological_reality).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_church_adjudication_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The teaching authority of the Roman Catholic Church. Declares and maintains the doctrine that marriage is an indissoluble sacrament constituted by the consent of the parties and witnessed by the church. This reading holds that the bond persists as an ontological fact independent of human desire, circumstance, or pastoral compassion. The magisterium defends this doctrine as continuous with apostolic tradition and essential to the sacrament's meaning. Enforces it through the canonical tribunal system and denial of sacramental participation to those who attempt to dissolve the bond. Has near-complete exit flexibility: can reinterpret doctrine at will without suffering personal cost (Vatican II example).
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, magisterium, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The ecclesiastical courts that adjudicate marriage cases, primarily through annulment petitions. Administer the doctrine by investigating whether a valid sacramental bond was formed at the moment of consent. Collect evidence, hear testimony, render decisions, levy costs. Their existence and operation depend on treating marriage as a fixed legal/sacramental status requiring expert hierarchical adjudication. Staffed by trained canon lawyers, priests, and church officials whose professional identity and career paths depend on the legitimacy and necessity of this adjudicatory function. Have analytical-level exit: they implement doctrine but do not set it; their power derives from institutional mandate.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunal_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Catholics whose first marriage has dissolved civilly but whose first spouse still lives. Bound by the sacramental bond regardless of civil dissolution. If they seek to remarry in the church, they must petition for annulment — a declaration that the first bond was never sacramentally valid. Face substantial barriers: the annulment process is costly (tribunal fees, required documentation, canonical representation), lengthy (months to years), outcome-uncertain (approval not guaranteed), and requires detailed testimony about marriage's internal psychological state at initial consent. Their Catholic identity (family, community, sacramental practice) makes exit from the church institutionally painful. Bear the full cost of attempting to regularize their situation without control over the criteria for success.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_catholics_seeking_remarriage, payer,
    powerless, biographical, identity_locked, local).

% Catholics in a second marriage (or subsequent marriage) who either lack an annulment of their first marriage or whose annulment petition was denied. In an objectively unlawful state — they are bound by the first sacramental bond and cannot form a second valid sacrament. Barred from Eucharist and most sacramental access. Their situation is one of enforced exclusion from full ecclesial participation, justified on the grounds that their living situation contradicts the sacramental order. The exclusion is structural and unambiguous: they cannot reform their situation without dissolving the second marriage (which may be where children live and emotional commitments lie) and obtaining an annulment of the first (which may be impossible if the first spouse refuses to cooperate or dies). Their identity as Catholics is maintained but truncated.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, remarried_catholics_without_annulment, payer,
    powerless, biographical, identity_locked, local).

% The ex-spouses of those seeking annulment or remarriage. Have a stake in defending the sacramental bond: the church treats the bond as binding on both parties, and an annulment declaration implies their first spouse's consent was defective. Not part of the official annulment proceeding (the petitioner and the church are the parties), but their interests are affected. Might object to an annulment petition or refuse to cooperate with it, effectively slowing or blocking the process. Their exclusion from formal decision-making power means their perspective — that the marriage was real, that the bond should persist — is heard if at all only as testimony, not as binding. The hierarchical reading privileges the institutional axis (church authority) over the relational axis (both persons' continued assent).
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, first_spouses_unreconciable_to_remarriage, excluded,
    moderate, biographical, constrained, local).

% Priests and bishops who encounter divorced Catholics seeking remarriage and who understand the pastoral situation as requiring compassion rather than strict legal adjudication. Are officially excluded from redefining the doctrine: they must counsel divorced Catholics to pursue annulment or remain unmarried. Some attempt workarounds (recommending pastoral solutions, tolerating de facto participation), but these are circumventions of the official rule, not legitimate expressions of it. Their pastoral judgment is subordinated to hierarchical doctrine. Bear a professional cost (cognitive dissonance, the pain of denying sacraments to those they serve) without power to change the structure.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_clergy_supporting_remarriage, excluded,
    moderate, biographical, constrained, local).

% The formal teaching body and administrative councils of the Roman Catholic Church at the highest level. Have the authority to reinterpret or revise doctrine, though such revision is treated as exceptional and requiring substantial justification. Monitor the implementation of the indissolubility teaching and could in principle declare this reading obsolete or modify its enforcement. From this analytical seat, the constraint's structure is fully visible and subject to deliberate decision.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, vatican_magisterium_council, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunal_apparatus).
narrative_ontology:fixing_cost_class(marriage_sacrament__hierarchical_indissolubility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures a shared understanding of marriage as a binding covenant witnessed and validated by the church institution. Provides a unified framework for understanding what counts as a valid marriage, and establishes hierarchical adjudication as the authoritative mechanism for resolving contested cases. Coordinates the entire Catholic community around a single doctrine of marriage's nature and indissolubility.
% TRANSFER_FUNCTION: Transfers exclusion from full sacramental participation (Eucharist access, blessing of remarriage, public standing as a communicant) from divorced/remarried Catholics to the institutional church, which gains moral authority, doctrinal coherence, and the power to adjudicate religious status. Transfers labor and costs (tribunal operations, doctrinal defense, pastoral anxiety) from laypeople to the institutional clergy and legal apparatus.
% ABSENT_VOICES: First spouses whose relational perspective on the marriage would complicate the ontological framing (their continued understanding of the bond's reality or dissolution) are excluded from formal proceedings. Pastoral clergy whose experience suggests the ideal of indissolubility clashes with human flourishing are constrained from redefining doctrine. Those who left the church entirely after divorce are structurally absent from this conversation — the constraint is most visible to those still seeking participation.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared — if the church abandoned the hierarchical ontological reading and permitted remarriage with pastoral discretion — the entire sacramental status of millions of divorced Catholics would shift overnight. Remarried Catholics would gain full access to Eucharist. Tribunals would contract sharply (fewer annulment cases to adjudicate). The church's doctrinal coherence would reorganize around a different reading of marriage's nature. Pastoral relationships would restructure around compassion rather than legal compliance. The constraint is constitutive of the actual configuration; its removal is not a minor adjustment but a reordering.
% FOUNDING_PROBLEM: In the early Christian community and medieval church, marriage required stable, indissoluble commitment to build stable households and secure inheritance lines; the church's role was to witness and validate these bonds as sacramental reality. Over time, the teaching articulated that the bond's indissolubility is not contingent on the parties' continued relationship but intrinsic to the sacrament's nature — Christ's bond to the church is indissoluble, and marriage mirrors this. This reading constitutionalizes that doctrine: the bond is an objective sacramental fact, not a relationship-dependent commitment.
% FOUNDING_PROBLEM_CORROBORATION: The magisterium attests the founding problem is live: stable sacramental understanding of marriage remains essential to Catholic identity. Vatican II reaffirmed indissolubility as doctrine. However, pastoral theologians, marriage counselors, and divorced Catholic organizations attest the founding problem has shifted: modern marriages are now about emotional partnership, not inheritance or household stability; sacramental witness no longer requires legal indissolubility to be meaningful; the 'problem' the constraint solves is no longer the stated problem, but institutional control of religious status and doctrinal authority. Post-Vatican II reforms (expansion of annulment grounds, pastoral emphasis on mercy) indicate even within the teaching authority some contestation that the founding problem persists in its original form.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) and rising over the interval (0.48→0.68), indicating increasing institutional reliance on the constraint to justify and maintain the tribunal apparatus. Suppression is high (0.72) because the constraint's persistence depends on actively excluding rival interpretations (the pastoral reading is marginalized, not integrated), restricting access to remarriage, and denying sacraments to those who transgress. Theater is low-to-moderate (0.28) because the doctrine is genuinely believed by the magisterium and a significant portion of the lay Catholic population — but the measurement's rise (0.08→0.28) suggests that as attendance declines and pastoral pressure increases, more of the enforcement effort goes into defending the doctrine's legitimacy rather than its practical operation. The time grid is aligned: all three metrics measured at the same seven time points (0, 8, 16, 24, 32, 40, 50), allowing the engine to compute drifts and detect coupling without time-series misalignment.
 *
 * PERSPECTIVAL GAP:
 *   From the magisterium's and tribunal apparatus's analytical seat, the constraint is genuine coordination: it clarifies what marriage is, provides a stable framework for sacramental participation, and offers a means (annulment) for those in error to regularize their status. From the divorced/remarried Catholic seats, it is extraction: the clarification comes at their expense, the framework excludes them, and the 'means' for regularization is in the hands of authorities who benefit from its scarcity. From pastoral clergy, it is internal contradiction: the doctrine is true, but the enforcement is unjust. The engine computes these divergent classifications from the structural data: the magisterium and tribunal apparatus have near-zero effective extraction (they benefit, have power, control exit); the divorced Catholics have high effective extraction (they are targets, powerless, identity-locked). The analytical seat sees the full asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The magisterium and tribunal apparatus are beneficiaries with powerful, institutional, arbitrage-grade exit (they can reinterpret doctrine at will and suffer no cost from doing so — they are the authority). Their directionality is near 0.0 (full beneficiary): they benefit from doctrine maintenance and have the structural power to change it. Divorced/remarried Catholics are victims with powerless positions, identity-locked exit (the church is their community and identity; leaving is psychologically costly even though structurally possible), and no seat at the adjudicatory table. Their directionality is near 1.0 (full target): they bear the extraction fully and have minimal escape without identity-loss. Pastoral clergy sit intermediate: they benefit from a clear doctrine (it simplifies their role) but are harmed by the enforcement (it creates pastoral pain and cognitive dissonance). Their directionality is moderate (around 0.5). The first spouses are excluded rather than coordinated — they appear in the narrative but do not carry a directionality value because they are outside the formal constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is tangled_rope: it coordinates understanding of marriage as sacrament (genuine coordination function, beneficiaries=clergy, magisterium) AND extracts exclusion from sacramental participation from those unable to obtain annulment (asymmetric extraction, victims=divorced/remarried Catholics). The enforcement is active: the church must actively deny Eucharist, defend the doctrine against pastoral critique, and maintain the tribunal apparatus. The extraction is not incidental to the coordination — it is the mechanism by which the magisterium maintains its authority to define religious status. The founding problem (securing stable understanding of marriage) is live at the doctrinal level but shows signs of decay at the institutional level: Vatican II's pastoral reforms suggest even the magisterium is adjusting the doctrine to reduce extraction (expanding annulment grounds is a de facto acknowledgment that some marriages cannot be held as binding). The rising theater_ratio (0.08→0.28) indicates that increasingly, the constraint's operation is performative: defending the doctrine's legitimacy rather than its practical operation. This combination (real coordination + asymmetric extraction + rising theater + founding problem decay) marks the constraint as vulnerable to reclassification as snare if the coordination function continues to erode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_locked_structural_vs_internalized,
    'Is the identity-lock binding divorced Catholics to this constraint primarily structural (institutional exclusion, sacramental denial, social stigma) or internalized (the agents have fused their sense of being Catholic with accepting the doctrine, such that exit from the church resolves the constraint but the psychological suppression persists)?',
    'Post-exit trajectory: survey divorced Catholics who have left the church entirely (to another denomination or no religion) and measure whether the subjective sense of exclusion/shame persists, or whether it decays as they cease to encounter the institutional barrier. Internalization is persistent; structural suppression is not.',
    'If primarily structural, the constraint''s effective suppression is bounded by the exit rate; if internalized, divorced Catholics carry the suppression with them and are recapturable by the institutional church even if formal rules change. This affects the classification boundary: a structurally-suppressed snare can be converted to rope by removing enforcement; an internalized snare persists through belief systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_structural_vs_internalized, empirical, 'Whether suppression persists after structural removal.').

omega_variable(
    hierarchical_adjudication_necessity,
    'Is hierarchical tribunal adjudication structurally necessary for the sacramental indissolubility doctrine to function, or is it an institutional apparatus that _claims_ necessity but could be replaced by different adjudicatory mechanisms (e.g., pastoral discernment, peer witness, congregational consensus)?',
    'Compare this reading (hierarchical, tribunal-centric) with the sibling civic_pastoral_reading''s actual implementation in Catholic communities (some dioceses emphasize pastoral approach; some bishops have quietly expanded annulment grounds or widened access to Eucharist for remarried). If the doctrine persists and functions under less hierarchical adjudication, the tribunal apparatus is institutional capture, not structural necessity.',
    'If adjudication is replaceable, the constraint is tangled_rope (coordination of marriage understanding + extraction via institutional control) that could be converted to rope by restructuring who adjudicates. If adjudication is necessary, the hierarchy is integral to the coordination function itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hierarchical_adjudication_necessity, conceptual, 'Whether the hierarchical tribunal is necessary or an institution claiming necessity.').

omega_variable(
    ontological_vs_normative_foreclosure,
    'This reading treats marriage as an ontological fact (the bond _is_ indissoluble). The sibling civic_pastoral_reading treats indissolubility as normative (the bond _should be_ indissoluble, but human failure permits pastoral mercy). Can both readings coexist as live institutional positions, or does the ontological claim logically foreclose the normative claim?',
    'Church authority formally defines whether indissolubility is constitutive (ontological) or aspirational (normative). If both are embraced simultaneously in official teaching, coexistence holds. If one is formally declared binding and the other relegated to opinion, foreclosure applies.',
    'Foreclosure would indicate genuine incompatibility; coexistence would indicate institutional tension but structural permissibility. This determines the relation type in cs_structure.reading_relations: forecloses vs. coexists_with.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontological_vs_normative_foreclosure, conceptual, 'Logical relationship between ontological and normative readings.').

omega_variable(
    founding_problem_decay_and_mandatrophy,
    'The founding problem is stated as securing shared understanding of marriage as sacrament. Has this problem shifted such that the constraint now primarily solves an institutional problem (maintaining magisterial authority to define religious status) rather than the original doctrinal problem?',
    'Compare magisterial investment in indissolubility doctrine before and after Vatican II. If Vatican II''s pastoral openings (expanded annulment grounds) were adopted to address the founding problem, it persists. If they were resisted by conservatives because they weaken magisterial authority, the constraint has drifted toward capturing institutional power rather than serving coordination.',
    'Founding problem decay + rising theater ratio together indicate mandatrophy: the justification (stable understanding) no longer tracks the operation (institutional control). This would support reclassification as snare rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_decay_and_mandatrophy, empirical, 'Whether the founding problem is still the constraint''s primary function.').

omega_variable(
    suppression_mechanism_structural_internalized_fusion,
    'For divorced Catholics identity-locked to this constraint, what portion of the suppression is structural (barriers they encounter) vs. internalized (values they have adopted, such that they suppress themselves from seeking remarriage or full participation)?',
    'Compare stated reasons divorced Catholics give for not remarrying or not seeking annulment: structural reasons (cost, delay, risk of rejection, institutional barriers) vs. internalized reasons (belief that remarriage is wrong, that the sacramental bond is real and indissoluble). A therapy or pastoral intervention that clarifies their agency might shift the ratio.',
    'If internalized suppression is high, the constraint carries with it a belief system that persists even if external barriers are removed. If structural suppression is high, removing barriers (free annulment, pastoral access) would resolve the constraint more cleanly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_internalized_fusion, empirical, 'Whether suppression is self-imposed or externally enforced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t8, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(marr_tr_t8, observed).
narrative_ontology:measurement(marr_tr_t16, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(marr_tr_t16, observed).
narrative_ontology:measurement(marr_tr_t24, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 24, 0.21).
narrative_ontology:measurement_basis(marr_tr_t24, observed).
narrative_ontology:measurement(marr_tr_t32, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement_basis(marr_tr_t32, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement_basis(marr_tr_t40, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(marr_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t8, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(marr_be_t8, observed).
narrative_ontology:measurement(marr_be_t16, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement_basis(marr_be_t16, observed).
narrative_ontology:measurement(marr_be_t24, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(marr_be_t24, observed).
narrative_ontology:measurement(marr_be_t32, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(marr_be_t32, observed).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(marr_be_t40, observed).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(marr_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t8, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(marr_su_t8, observed).
narrative_ontology:measurement(marr_su_t16, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement_basis(marr_su_t16, observed).
narrative_ontology:measurement(marr_su_t24, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement_basis(marr_su_t24, observed).
narrative_ontology:measurement(marr_su_t32, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(marr_su_t32, observed).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(marr_su_t40, observed).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(marr_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__hierarchical_indissolubility_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel is decomposed into two readings with structurally distinct ε values and beneficiary/victim sets. The hierarchical_indissolubility_reading (this file) instantiates the doctrine as ontological claim with high extraction (ε≈0.68) via institutional adjudication gatekeeping. The civic_pastoral_reading (sibling constraint) instantiates the doctrine as normative ideal with lower extraction (expected ε≈0.35–0.45) via pastoral discernment and reduced institutional enforcement. Both readings share the kernel (marriage is sacramental) but have different ε-referents: this reading measures extraction from the standing hierarchical-tribunal arrangement; the sibling reading measures extraction from a more pastoral, case-by-case arrangement. The readings are linked via network.affects_constraints because the hierarchical reading's institutional weight pressures the pastoral reading's legitimacy, and pastoral success (lower divorce stigma, greater Eucharistic access) creates downstream pressure on the hierarchical reading to adjust. They coexist as live institutional positions held by different dioceses and theological factions within the global Catholic Church.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
