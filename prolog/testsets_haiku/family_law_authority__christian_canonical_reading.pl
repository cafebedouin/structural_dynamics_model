% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Marriage as Sacrament Under Christian Ecclesiastical Authority
 *   domain: religious_governance/family_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the Christian canonical reading of the
 *   family_law_authority kernel—marriage as a sacrament under ecclesiastical
 *   authority, with particular emphasis on Catholic doctrine of
 *   indissolubility and the hierarchy's gatekeeping over validity, annulment,
 *   and divorced-person remarriage. The constraint sits in contestation with
 *   secular legal regimes (civil divorce), other religious traditions (Hindu
 *   dharma, Islamic sharia, Zoroastrian law), and increasingly with
 *   Protestant denominations whose own readings of Christian marriage permit
 *   divorce and remarriage. The ε-invariance principle requires this story to
 *   author one reading's ε stably: the extractiveness measured here is the
 *   extraction from the Catholic/Orthodox sacramental reading's own
 *   standpoint—the control the hierarchy exerts over marriage definition and
 *   dissolution. Other readings (secular contractual, Hindu, Islamic) would
 *   author different ε values for the same standing arrangement, because they
 *   assess it by their own lights. This story does not average across
 *   readings; it instantiates one.
 *
 * KEY AGENTS:
 *   - Ecclesiastical hierarchy (Catholic/Orthodox magisterium): sets doctrine, controls annulment, enforces sacramental discipline — institutional beneficiary
 *   - Doctrine of sacramental indissolubility: the non-agent vindicated proposition that marriage is an unbreakable covenant before God — legitimates the hierarchy's gatekeeping
 *   - Married couples committed to permanence: beneficiaries of the sacramental framing and the community's spiritual support; constrained exit (would require leaving faith)
 *   - Divorced persons: primary payers, identity-locked (Catholic identity bound up with church membership), face pastoral stigma and sacramental exclusion
 *   - Women in abusive marriages: trapped payers, compounded extraction (abuse + canonical inability to remarry/separate with dignity)
 *   - Interfaith couples: payers through marriage-validity contestation; marriages may be declared null on canonical grounds even if civilly valid
 *   - Pastoral theologians: excluded voices advocating reform, interior critique without binding authority
 *   - State legal authority: observer seat computing different constraint boundaries (civil divorce as legitimate, sacramental indissolubility as overreach)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.68).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Marriage as Sacrament Under Christian Ecclesiastical Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "religious_governance/family_law/political_theory").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '9009dc54-2e41-4773-a9ec-622eb4ef6dde').
narrative_ontology:cs_kernel_codification('9009dc54-2e41-4773-a9ec-622eb4ef6dde', fixed_text).
narrative_ontology:cs_authority_grounding('9009dc54-2e41-4773-a9ec-622eb4ef6dde', lineage).
narrative_ontology:cs_interpretation_layer_present('9009dc54-2e41-4773-a9ec-622eb4ef6dde').
narrative_ontology:cs_reading_relation('9009dc54-2e41-4773-a9ec-622eb4ef6dde', family_law_authority__secular_contractual_reading, coexists_with).
narrative_ontology:cs_reading_relation('9009dc54-2e41-4773-a9ec-622eb4ef6dde', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('9009dc54-2e41-4773-a9ec-622eb4ef6dde', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('9009dc54-2e41-4773-a9ec-622eb4ef6dde', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('9009dc54-2e41-4773-a9ec-622eb4ef6dde', foundational, marriage_is_sacrament_not_contract).
narrative_ontology:cs_axiom_status(marriage_is_sacrament_not_contract, holdable).
narrative_ontology:cs_axiom_grounding('9009dc54-2e41-4773-a9ec-622eb4ef6dde', marriage_is_sacrament_not_contract, deontological).
narrative_ontology:cs_axiom('9009dc54-2e41-4773-a9ec-622eb4ef6dde', foundational, sacramental_permanence_indissoluble_by_humans).
narrative_ontology:cs_axiom_status(sacramental_permanence_indissoluble_by_humans, holdable).
narrative_ontology:cs_axiom_grounding('9009dc54-2e41-4773-a9ec-622eb4ef6dde', sacramental_permanence_indissoluble_by_humans, theological).
narrative_ontology:cs_reference_frame('9009dc54-2e41-4773-a9ec-622eb4ef6dde', apostolic_sacramental_permanence).
narrative_ontology:cs_drift_state('9009dc54-2e41-4773-a9ec-622eb4ef6dde', contemporary_post_enlightenment_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9009dc54-2e41-4773-a9ec-622eb4ef6dde', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, doctrine_of_sacramental_indissolubility).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, divorced_persons_seeking_remarriage).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, women_bound_to_abusive_spouses).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, interfaith_couples_denied_validity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, married_couples_seeking_permanence).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces doctrine on marriage as sacrament, indissolubility, annulment conditions, and divorced-person remarriage eligibility. Controls the interpretive apparatus (canon law councils, papal teaching, episcopal authority) and exercises discipline through sacramental access and pastoral governance. Benefits from the authority this constraint grants and from the legitimacy of being the arbiter of a sacred institution.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchy, agenda_setter,
    institutional, civilizational, analytical, global).

% Receive the sacramental framing of marriage as a permanent covenant before God, validated by the church and the faith community. Experience genuine coordination benefit: the doctrine provides spiritual meaning, institutional blessing, community support, and a clear understanding of commitment as non-revisable. Their exit would require leaving the faith tradition entirely; most accept the permanence frame as aligned with their values.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, married_couples_seeking_permanence, beneficiary,
    moderate, generational, constrained, global).

% Cannot canonically remarry within the church without annulment, which is restricted to narrow grounds and processed through a secretive, costly machinery that often denies petitions. They are excluded from the Eucharist in many parishes, marked as living in sin or in 'irregular unions,' and face pastoral pressure or judgment. Civil divorce is available to them (in most jurisdictions), but accepting it means accepting the loss of sacramental status and community membership. Their Catholic identity is bound up with institutional belonging, making exit costly even when it is formally available.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, divorced_persons_seeking_remarriage, payer,
    powerless, biographical, identity_locked, global).

% Face compounded extraction: marital abuse and canonical inability to divorce and remarry without annulment. Separation from an abusive spouse is permitted; remarriage is not. Many experience pastoral messaging that emphasizes reconciliation and perseverance in marriage, which can delay or prevent escape. They have little structural voice in doctrinal deliberation about whether sacramental permanence serves them or harms them. They are largely invisible in official teaching, though the constraint operates most severely on them.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, women_bound_to_abusive_spouses, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, women_bound_to_abusive_spouses, excluded).

% Bear the constraint through marriage-validity contestation: a marriage between a Catholic and a non-Christian, or between a Catholic and a non-Catholic Christian, may be declared invalid by the church on technical grounds (form defects, dispensation violations, inadequate consent) even if it is civilly recognized and the couple considers it binding. The constraint's validity gates operate on their relationship status itself, not merely on divorce/remarriage. Their exit requires one partner converting to Catholicism, one accepting the marriage's non-recognition, or both leaving the faith. The constraint makes their marital status itself contestable.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, interfaith_couples, payer,
    moderate, biographical, constrained, global).

% A doctrinal proposition, not an agent: the claim that marriage, once sacramentally sealed by God before the church, is indissoluble by any human authority and remains binding even if the couple ceases to live together. This doctrine is vindicated and enforced by the ecclesiastical hierarchy. It collects no direct rents but legitimates the hierarchy's gatekeeping authority and provides the theological ground for denying remarriage to divorced persons.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, theology_of_sacramental_indissolubility, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(family_law_authority__christian_canonical_reading, theology_of_sacramental_indissolubility).

% Are present within the church structure but excluded from binding doctrine revision. They argue for pastoral accompaniment of divorced persons, broadened annulment grounds, or reconsideration of indissolubility itself based on Gospel emphasis on mercy and human dignity. Their voice is listened to in some forums (synods, theological conferences) but lacks authority to change official teaching. They remain institutionally constrained; they cannot exit without leaving the priesthood or religious life.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, pastoral_theologians_advocating_reform, excluded,
    moderate, biographical, constrained, global).

% Recognizes civil divorce and remarriage in parallel with or against ecclesiastical doctrine, depending on jurisdiction and legal tradition. Provides civil marriage dissolution and remarriage eligibility independent of church approval. Observes the constraint's operation and in some cases comes into direct conflict with it (e.g., when state law recognizes a remarriage but the church treats it as invalid). The observer seat documents the constraint's extraction most clearly: divorced persons can exit ecclesiastical authority via civil law, but at the cost of losing sacramental status.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, state_legal_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__christian_canonical_reading, ecclesiastical_hierarchy).
narrative_ontology:fixing_cost_class(family_law_authority__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes marriage as a sacramental covenant binding before God and the church community, providing spiritual meaning, institutional blessing of sexual union, community support for permanent commitment, and doctrinal clarity about what constitutes valid marriage and what dissolves it. This solves the coordination problem of defining marriage's nature and the church's role in it, and ensures doctrinal and pastoral unity within the faith community on a single standard for marriage validity and indissolubility.
% TRANSFER_FUNCTION: Moves control over marriage definition, validity assessment, divorce eligibility, and remarriage permission from individual conscience and civil law to ecclesiastical authority. Divorced persons transfer their remarriage eligibility to the church's annulment machinery; women in abusive marriages transfer their exit options to the church's definition of grounds for separation; interfaith couples transfer their marriage status to the church's validity tests. The ecclesiastical hierarchy collects institutional authority, doctrinal legitimacy, pastoral power, and the ability to police sexual and family life through this gatekeeping.
% ABSENT_VOICES: Divorced persons are partially present through pastoral structures but systemically excluded from doctrine revision—their lived experience of the constraint is not centered in magisterial deliberation. Women experiencing domestic abuse are largely absent, though the constraint's consequences for them are most severe. Interfaith couples' experience of validity contestation is not centered in ecclesiastical decision-making. Protestant reformers (present historically but now separated) are not in the conversation about what Christian marriage should be. Civil legal authorities and their assessments of marriage validity are not present in canonical deliberation. Secular philosophers and human-rights advocates are excluded by the constraint's self-enclosed authority frame.
% DISAPPEARANCE_RATIONALE: If ecclesiastical authority over marriage validity and divorced-person remarriage disappeared overnight, the reorganization would be immediate: divorced Catholics would remarry without annulment petitions; interfaith couples would marry without validity contestation; women in abusive marriages would separate and remarry without canonical impossibility; the pastoral apparatus managing 'irregular unions' would collapse; the church community's doctrinal unity around sacramental indissolubility would fragment into denominational and individual variation (the Protestant Reformation scenario, repeated). State and civil law would become the default authority for marriage definition and dissolution. The spiritual meaning of marriage would devolve to individual belief and denominational practice rather than universal ecclesiastical doctrine.
% FOUNDING_PROBLEM: Early Christian theology, grounded in Gospel passages (Matthew 19:6, Mark 10:9) and apostolic tradition, asserted that marriage is an indissoluble covenant sealed by God—not a merely human contract—and that the church participates in and witnesses this sacramental sealing. The founding problem was to establish marriage as a sacred, spiritually significant commitment beyond economic or social contract, to anchor church teaching against easy divorce, and to maintain pastoral unity and doctrinal clarity on what constitutes valid marriage and what severs it.
% FOUNDING_PROBLEM_CORROBORATION: The Catholic Church and traditional theological sources (Aquinas, Council of Trent, John Paul II, Francis's 2015 letters on marriage and annulment) attest the founding problem remains live: marriage is permanently sacramental and indissoluble, and the church must guard this doctrine. Protestant denominations from the Reformation onward (Lutheran, Reformed, Anglican, Pentecostal) attest the founding problem has been re-read: divorce is permissible when the marital covenant is broken (infidelity, abandonment, abuse), and the church's role is pastoral accompaniment and mercy, not indissolubility enforcement. Contemporary canon lawyers, divorced Catholics, women's advocacy organizations, and secular family law scholars outside the benefiting hierarchy attest the founding problem's original solution (indissolubility as spiritual protection and church authority) has become a mechanism of control, spiritual harm, and institutional gatekeeping. They argue the founding problem is no longer how to establish marriage as sacred (it is widely recognized as spiritually and socially significant), but how to honor marriage's sacred nature while also honoring human dignity and freedom when the covenant breaks.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurement series (1500–2026) tracks the constraint's evolution from pre-Reformation ecclesiastical monopoly through post-enlightenment pluralism. Base extractiveness rises from 0.45 to 0.68 over 526 years: as state law secularized and civil divorce became widely available, the church's gatekeeping became a choosing-constraint rather than a monopoly—targets could exit to civil law, but at the cost of spiritual status and community belonging. The identity-lock on 'Catholic identity' intensified the effective extraction (exit is available but costly). Suppression_requirement rises from 0.48 to 0.72: early enforcement relied on near-total ecclesiastical monopoly over marriage legitimacy; modern enforcement requires active pastoral discipline, confession-booth policing, sacramental withholding, and institutional pressure on family and community, because the underlying structural monopoly is gone. Theater_ratio rises from 0.12 to 0.41: the early constraint's function was largely authentic (the church really did adjudicate all marriage questions); modern theater includes annulment machinery that often rationalizes predetermined outcomes ('finding' nullity on technical grounds while the marriage was functionally a commitment), and pastoral 'accompaniment' language that masks continued exclusion. This is not a Goodhart drift where proxy goals replace real ones—the coordination function (defining marriage validity, supporting permanent commitment) remains real—but the ratio of enforcement activity devoted to the extraction gate (keeping divorced persons out) versus the coordination benefit (blessing permanent marriages) has grown. The constraint's life shows accumulating extraction under the same doctrinal label.
 *
 * PERSPECTIVAL GAP:
 *   From the ecclesiastical hierarchy's seat, the constraint is a tangled rope: it coordinates marriage as a sacred covenant (genuine coordination problem: early Christianity needed to establish sexual union as spiritually significant, not merely contractual), AND it extracts control over who can remarry and on what grounds (asymmetric extraction from divorced persons). From the divorced-person's seat, the constraint appears as a snare: the coordination function (permanence framing) was what they chose, but the extraction (inability to remarry while living) was imposed and persists even after civil law granted them exit—the pastoral framing of 'accompaniment' is theater masking continued exclusion. From the woman-in-abuse's seat, the constraint is even more purely extractive: the permanence framing offers her no protection, only a doctrinal barrier to escape. The engine computes these divergences per-seat from the structural data (power, exit, beneficiary/victim declarations); the authored claim (tangled rope) is independent of these computations, and the divergence is exactly what the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for the ecclesiastical hierarchy is near-zero (full beneficiary): the constraint exists to expand and legitimate their authority. They set the rules, control the adjudication, collect institutional power. Directionality for divorced persons is near 1.0 (full target): they pay through spiritual status loss, sacramental exclusion, and pastoral discipline; their exit (civil remarriage) does not resolve the extraction (the church still denies them sacraments and community in-group status). Identity-locked exit raises their effective directionality: they cannot exit without losing the identity frame ('being Catholic') that constitutes them. Women in abuse face even sharper extraction: they are trapped (not merely identity-locked) and pay the highest cost. For married couples seeking permanence, directionality is nearer 0.5 (symmetric): they receive the sacramental framing as something they choose, which aligns with their values, and they also pay the cost that if the marriage breaks, they cannot remarry—but this is a symmetric bargain they largely entered voluntarily. Interfaith couples are near 1.0: the validity contestation is imposed on them, and their exit (one partner converting, or accepting the marriage as non-recognized) is costly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing marriage as a sacred, permanent covenant) was substantially solved by the end of the medieval period within Christian theology: the church had defined marriage as a sacrament, established permanence as doctrine, and embedded it in canon law. By 1500, the constraint was no longer solving a live coordination problem within Christendom—it was administering a solved problem. The Protestant Reformation explicitly challenged the problem-statement (marriage is NOT indissoluble; divorce is permissible; remarriage is pastoral accompaniment, not violation), and the Enlightenment introduced secular state authority over marriage (solving the coordination problem through civil law, not ecclesiastical doctrine). By 1800, the founding problem was dead in much of the Christian world: marriage definition and dissolution had migrated to state law. The constraint persisted, and the measurement series captures the accumulating extraction: as the problem died, the constraint did not dissolve but mutated into a pure gatekeeping mechanism. The ecclesiastical hierarchy's continued enforcement of indissolubility after the founding problem was dead (or was redefined by Protestants and secular law) is the signature of mandatrophy: the constraint's original justification no longer applies, but institutional inertia and the hierarchy's interest in preserving its authority keep it alive. The rising extraction score (0.45→0.68) and rising theater ratio (0.12→0.41) document this trajectory: the constraint is doing less genuine coordination work and more theatrical maintenance of authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_disciplinary_reading,
    'Is the indissolubility doctrine primarily a sacramental-theological claim about the nature of marriage before God (marriage cannot be unmade because God seals it), or primarily a disciplinary-institutional mechanism for maintaining ecclesiastical control and doctrinal uniformity?',
    'Textual-historical analysis of magisterial documents and canon law development: do the foundational texts (Scripture, Aquinas, Council of Trent, Vatican II) center on the spiritual reality of God''s sealing, or on the church''s institutional authority to police dissolution? Cross-reading with Protestant reformers'' explicit critique (divorce reflects broken covenants, not God''s unsealing).',
    'If primarily sacramental, the constraint is grounded in a genuine theological claim (about divine action), and its extractiveness is partly the cost of maintaining the church''s role as sacramental witness. If primarily disciplinary, the constraint is recognizable as institutional rent-seeking dressed in theological language. The distinction affects whether limiting or reforming the constraint requires doctrinal change (reframing what God does) or only institutional reform (opening the annulment process, including remarried Catholics in sacraments).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_disciplinary_reading, conceptual, 'Whether indissolubility is metaphysical claim or institutional control mechanism.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of divorced-persons'' remarriage primarily structural (external gatekeeping by annulment machinery, pastoral discipline, sacramental withholding, family/community pressure mediated by church authority) or primarily internalized (guilt, shame, internalized identity as ''failed,'' doubts about one''s own worth and spiritual standing)?',
    'Qualitative research on post-exit trajectories: do divorced Catholics who leave the faith report lasting suppression effects and identity damage? Do those who remain active in the faith report suppression as primarily external (institutional barriers they could overcome if the rules changed) or internal (conviction that the church is right, or deep shame that persists regardless of rules)?',
    'If primarily structural, the constraint is more readily modifiable by institutional policy change (broadening annulment grounds, sacramental inclusion). If primarily internalized, the constraint persists even after external mechanisms relax, requiring longer-term pastoral and spiritual identity reconstruction. The measured suppression score may underestimate the constraint''s effective suppression on identity-locked targets if a large portion is internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Locus of suppression in identity-locked exit case: external barriers or internal conviction.').

omega_variable(
    christian_canonical_fragmentation,
    'Does ''Christian canonical reading'' name a coherent doctrine held across denominations, or has it fragmented into Catholic, Orthodox, and Protestant Christianities that no longer share a single kernel for family_law_authority?',
    'Examine whether Catholic, Orthodox, and major Protestant traditions (Lutheran, Reformed, Anglican, Pentecostal) are still debating the same kernel question (Is marriage a sacrament? Can it be dissolved? Who has authority?) in a unified discourse, or whether they have splintered into separate Christianities with incommensurable frameworks. If Protestant traditions have defined a separate authority answer (pastoral accompaniment and divorce are permissible), do they still recognize themselves as reading the same Christian marriage doctrine?',
    'If the reading is coherent despite denominational variance, the constraint story captures one reading with internal denominational variation (Catholic indissolubility vs. Protestant divorce permission as competing interpretations of Christian marriage). If it has fragmented, the constraint should decompose into Catholic_sacramental_reading and Protestant_covenant_reading as separate constraints, each with its own kernel or its own family of kernels. The framework''s granularity depends on whether Christian traditions still share a single family_law_authority kernel or have split into separate kernel families.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(christian_canonical_fragmentation, conceptual, 'Whether Christian denominations share a single kernel or have fragmented into separate kernel families.').

omega_variable(
    state_law_override_asymmetry,
    'When state law permits divorce and remarriage but ecclesiastical law forbids it, which authority is doing the extracting: the church (by withholding sacramental status from civil divorces and remarriages) or the state (by overriding religious authority over marriage)?',
    'Perspective-dependent structural analysis. From the church''s seat: the state is illegitimately overriding sacred law and imposing secular contract-marriage on the faithful, so the extraction flows from state override. From the divorced person''s seat: the church is extracting control over spiritual status even after civil law granted them exit, so the extraction flows from church gatekeeping. From the secular authority''s seat: the church is attempting to override civil law and deny citizens their legal rights, so the extraction flows from church power overreach. No single objective resolution—different seats compute different constraint boundaries and different extractiveness assessments.',
    'Affects whether the constraint is classified as sacramental_permanence (church enforcing its own religious doctrine) or ecclesiastical_authority_capture (church extracting continued control over status after state law provided exit). The per-seat classification divergence is precisely the measurement the framework exists to detect and quantify—not to resolve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_law_override_asymmetry, preference, 'Observer-seat-dependent framing of extraction locus when state and church law diverge.').

omega_variable(
    interfaith_validity_as_decomposition_boundary,
    'Are the validity conditions for interfaith marriages (baptism status, form requirements, dispensation procedures) structurally part of this same sacrament-indissolubility constraint, or do they constitute a separate constraint on interfaith-marriage recognition?',
    'Analyze whether the validity gates operate from the same doctrinal source (sacramental permanence requires valid sacramental formation) or a separable doctrine (marriage validity has independent grounds beyond indissolubility). If separable, interfaith-marriage validity should be authored as a sibling constraint in the family_law_authority kernel.',
    'If separable, this constraint story focuses on divorce/remarriage barriers; interfaith validity becomes a distinct constraint (family_law_authority__christian_canonical_interfaith_reading). If inseparable, the authoring must account for two structurally distinct payer groups (divorced persons and interfaith couples) with different extraction mechanisms and exit options. The ε-invariance principle suggests decomposition: the referent (ecclesiastical control over marriage) and the extraction mechanism (barrier to remarriage vs. barrier to recognition) are different enough to warrant separate constraint stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interfaith_validity_as_decomposition_boundary, conceptual, 'Whether interfaith validity gates are part of this constraint or a separate one.').

omega_variable(
    mandate_decay_and_zombie_persistence,
    'Does the founding problem—establishing marriage as a sacred, permanent covenant binding before God—remain live, or has it been dead since the post-Enlightenment secularization of marriage law?',
    'Assess whether the church''s continued enforcement of indissolubility serves the original coordination function (defining marriage as sacred) or persists primarily through institutional inertia and authority-maintenance. If the founding problem is dead in secular legal contexts and substantially reframed by Protestant denominations, does the constraint''s persistence indicate mandatrophy (a solved problem''s institutional apparatus continuing to extract)?',
    'If the founding problem is dead, the constraint is classified as piton (atrophied function persisting by inertia and institutional maintenance). If live, it is tangled rope (genuine coordination + asymmetric extraction). The measurement series (rising extraction, rising theater, stable suppression over 500 years) suggests mandatrophy: the constraint''s original function is largely solved, but the extraction gates persist. This omega names the uncertainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_decay_and_zombie_persistence, conceptual, 'Whether the founding problem is live or dead, indicating mandatrophy vs. tangled rope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 1500, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t1500, family_law_authority__christian_canonical_reading, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(fami_tr_t1650, family_law_authority__christian_canonical_reading, theater_ratio, 1650, 0.16).
narrative_ontology:measurement(fami_tr_t1800, family_law_authority__christian_canonical_reading, theater_ratio, 1800, 0.21).
narrative_ontology:measurement(fami_tr_t1950, family_law_authority__christian_canonical_reading, theater_ratio, 1950, 0.31).
narrative_ontology:measurement(fami_tr_t1990, family_law_authority__christian_canonical_reading, theater_ratio, 1990, 0.37).
narrative_ontology:measurement(fami_tr_t2026, family_law_authority__christian_canonical_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(fami_be_t1500, family_law_authority__christian_canonical_reading, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement(fami_be_t1650, family_law_authority__christian_canonical_reading, base_extractiveness, 1650, 0.52).
narrative_ontology:measurement(fami_be_t1800, family_law_authority__christian_canonical_reading, base_extractiveness, 1800, 0.58).
narrative_ontology:measurement(fami_be_t1950, family_law_authority__christian_canonical_reading, base_extractiveness, 1950, 0.63).
narrative_ontology:measurement(fami_be_t1990, family_law_authority__christian_canonical_reading, base_extractiveness, 1990, 0.66).
narrative_ontology:measurement(fami_be_t2026, family_law_authority__christian_canonical_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t1500, family_law_authority__christian_canonical_reading, suppression_requirement, 1500, 0.48).
narrative_ontology:measurement(fami_su_t1650, family_law_authority__christian_canonical_reading, suppression_requirement, 1650, 0.56).
narrative_ontology:measurement(fami_su_t1800, family_law_authority__christian_canonical_reading, suppression_requirement, 1800, 0.61).
narrative_ontology:measurement(fami_su_t1950, family_law_authority__christian_canonical_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(fami_su_t1990, family_law_authority__christian_canonical_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(fami_su_t2026, family_law_authority__christian_canonical_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__secular_contractual_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, family_law_authority__parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the family_law_authority kernel. The kernel itself is contested: different traditions and legal systems read the authority to define and dissolve marriage differently. This Christian canonical reading asserts ecclesiastical jurisdiction over marriage as a sacrament; the secular contractual reading asserts individual/state jurisdiction over marriage as a civil contract; Hindu, Islamic, and Parsi readings assert their respective religious authority structures. Each reading instantiates a different constraint with a different ε (each assessed by the reading's own lights, not by a neutral observer). The five constraints form a constraint family linked by network.affects_constraints. They do not compete to be the true constraint; rather, they document how different traditions read the same kernel question (authority over marriage) and thereby measure each other's extractiveness. The network edges enable contamination and legitimacy-structure analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
