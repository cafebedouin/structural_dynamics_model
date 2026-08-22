% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__civic_pastoral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__civic_pastoral_reading, []).

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
 *   constraint_id: marriage_sacrament__civic_pastoral_reading
 *   human_readable: Marriage Sacrament: Civic-Pastoral Reading (Discernment Over Doctrine)
 *   domain: religious/political/social
 *
 * SUMMARY:
 *   The Catholic Church's teaching on marriage indissolubility is a contested
 *   kernel instantiated through multiple readings. This constraint
 *   instantiates the CIVIC-PASTORAL READING: Marriage is understood as a
 *   lived pastoral relationship subject to human failure; indissolubility
 *   functions as an ideal and aspirational commitment rather than an
 *   ontological reality that cannot be dissolved. Pastoral authority
 *   (bishops, confessors) exercises discretionary discernment in individual
 *   cases, balancing doctrinal commitment to permanence against compassionate
 *   recognition of broken marriages and the spiritual needs of the civilly
 *   remarried faithful. This reading gained institutional momentum
 *   particularly from 2013 onward through synodal language, papal emphasis on
 *   mercy, and practical consecratorial loosening of annulment processes. The
 *   sibling HIERARCHICAL-INDISSOLUBILITY READING treats indissolubility as
 *   constitutive and ontological, requiring centralized doctrinal
 *   adjudication and uniform enforcement — a reading held by conservative
 *   custodians and traditional faithful. The two readings coexist in
 *   institutional tension; neither has formally foreclosed the other within
 *   the Church's single framework, but they generate different
 *   classifications of the constraint's structure. This story models the
 *   civic-pastoral reading as a tangled rope: it coordinates real pastoral
 *   need (mercy for the remarried) through an extractive mechanism (authority
 *   diffusion that creates normative instability for those depending on
 *   doctrinal clarity) whose persistence requires ongoing enforcement of
 *   discretion against formal doctrinal reaffirmation.
 *
 * KEY AGENTS:
 *   - pastoral_authority_discretionary_seat: Diocesan bishops and confessors who administer marriage doctrine through sacramental practice and pastoral counseling; benefit from expanded interpretive authority (d near 0.2)
 *   - traditional_identity_catholics: Faithful whose self-understanding depends on doctrinal clarity and sacramental consistency; bear cost of relativization and enforcement inconsistency; identity-locked (d near 0.85)
 *   - civilly_remarried_faithful: Experience pastoral recognition and case-by-case mercy; remain constrained and uncertain; secondary victims of system inconsistency (d near 0.65)
 *   - traditional_magisterium_custodians: Vatican offices and conservative hierarchs; experience institutional authority erosion; constrained by synodal momentum (d near 0.72)
 *   - synodal_reform_advocates: Theologians and lay movements promoting contextual mercy; benefit from pastoral reading as validation of reform agenda (d near 0.35)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.58).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.61).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage Sacrament: Civic-Pastoral Reading (Discernment Over Doctrine)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious/political/social").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '7821f591-4672-4d8a-b5da-41a7cd233b17').
narrative_ontology:cs_kernel_codification('7821f591-4672-4d8a-b5da-41a7cd233b17', fixed_text).
narrative_ontology:cs_authority_grounding('7821f591-4672-4d8a-b5da-41a7cd233b17', lineage).
narrative_ontology:cs_interpretation_layer_present('7821f591-4672-4d8a-b5da-41a7cd233b17').
narrative_ontology:cs_reading_relation('7821f591-4672-4d8a-b5da-41a7cd233b17', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('7821f591-4672-4d8a-b5da-41a7cd233b17', foundational, indissolubility_as_aspirational_ideal).
narrative_ontology:cs_axiom_status(indissolubility_as_aspirational_ideal, holdable).
narrative_ontology:cs_axiom_grounding('7821f591-4672-4d8a-b5da-41a7cd233b17', indissolubility_as_aspirational_ideal, deontological).
narrative_ontology:cs_axiom('7821f591-4672-4d8a-b5da-41a7cd233b17', foundational, pastoral_discretion_as_legitimate_authority).
narrative_ontology:cs_axiom_status(pastoral_discretion_as_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('7821f591-4672-4d8a-b5da-41a7cd233b17', pastoral_discretion_as_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('7821f591-4672-4d8a-b5da-41a7cd233b17', merciful_pastoral_accompaniment).
narrative_ontology:cs_drift_state('7821f591-4672-4d8a-b5da-41a7cd233b17', post_synod_on_family_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('7821f591-4672-4d8a-b5da-41a7cd233b17', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_authority_discretionary_seat).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_identity_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, civilly_remarried_faithful).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, civilly_remarried_faithful).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, synodal_reform_advocates).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_magisterium_custodians).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, mercy_over_rigid_law).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, pastoral_discernment_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diocesan bishops and confessors who administer marriage doctrine through sacramental practice and pastoral counseling. In this reading, they hold discretionary authority to discern individual conscience and pastoral need, balancing indissolubility as ideal against compassionate recognition of human failure. They set the de facto standard through case-by-case decisions, synodal guidance, and pulpit teaching that emphasizes mercy and personal circumstance over doctrinal uniformity. They benefit from expanded interpretive authority and institutional flexibility — the ability to respond pastorally without formal doctrinal change.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_authority_discretionary_seat, agenda_setter,
    institutional, generational, arbitrage, global).

% Faithful whose self-understanding as Catholic is constituted through fidelity to doctrinal clarity and sacramental consistency. They depend on publicly stable, predictable teaching about marriage indissolubility to maintain the coherence of their faith identity. In this reading, they experience doctrinal relativization (indissolubility presented as ideal rather than constitutive reality) and enforcement inconsistency (some receive communion after remarriage, others do not; some bishops grant annulments readily, others rarely). They bear the cost of lost normative clarity and institutional unreliability without controlling the interpretive frame that justifies the shift. Identity-locked because exit would require abandoning the Catholic identity itself, not merely changing parishes.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_identity_catholics, payer,
    moderate, generational, identity_locked, global).

% Catholics whose first marriage ended in civil divorce and who have remarried outside the Church (or within it after an annulment process). In this reading, they benefit from pastoral recognition that their remarriage reflects genuine human commitment despite the prior failure, from confessor discretion to admit them to communion in some cases, and from language emphasizing the pastoral nature of the Church's response rather than juridical condemnation. They remain constrained because the arrangement is discretionary (mercy extended case-by-case by individual confessors) rather than systematic, creating both opportunity and vulnerability to inconsistency. They pay through continued social marginalization and uncertainty about their sacramental standing.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, civilly_remarried_faithful, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, civilly_remarried_faithful, payer).

% Vatican doctrinal offices, theological councils, and conservative hierarchs who hold institutional authority to defend doctrinal boundaries. In this reading, they experience their authority eroded by pastoral discretion operating semi-autonomously below the level of formal doctrine. They pay through institutional fragmentation (uneven application of doctrine across dioceses and confessionals) and loss of authoritative clarity they previously maintained. They remain constrained because the pastoral reading has gained institutional momentum (synodal processes, papal rhetoric emphasizing mercy) without formal doctrinal revision, leaving them defending a position presented as rigid rather than custodial.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_magisterium_custodians, payer,
    institutional, generational, constrained, global).

% Catholics who divorced and remarried civilly without Church involvement (no annulment petition, no pastoral engagement). They are structurally excluded from the discretionary conversation — the pastoral reading assumes engagement with confessors and diocesan processes. They remain on the canonical periphery regardless of the reading's compassion. Their voices would argue for systematic inclusion rather than discretionary mercy.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, divorced_and_remarried_outside_church, excluded,
    powerless, biographical, trapped, global).

% Reform-oriented theologians, bishops, and lay movements (particularly in Europe and North America) who promote synodality and contextual mercy as legitimate expressions of Catholic tradition. They benefit from the pastoral reading as institutional validation of their reform agenda and expansion of interpretive authority beyond Rome. They have modest but real mobility — they can operate in some dioceses more comfortably than others, and the synodal conversation provides institutional venues they previously lacked.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, synodal_reform_advocates, beneficiary,
    organized, generational, mobile, global).

% The papal magisterium and Roman Curia offices charged with maintaining doctrinal coherence across the global Church. In this reading, they occupy an ambiguous position: they have nominally endorsed compassionate pastoral discernment (particularly under the recent papacy), but they retain the formal authority to reaffirm doctrinal strictness or to curtail diocesan discretion. They observe the constraint's operation and hold the power to alter it, but face institutional pressure not to revert to the pre-reading framework without forfeiting claims to pastoral sensitivity.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, vatican_doctrinal_authority, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, pastoral_authority_discretionary_seat).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates sacramental practice, pastoral counseling, and conscience formation across a global institutional Church by delegating case-by-case discernment to local pastoral authority rather than requiring uniform application of a fixed doctrinal formula. Solves the problem of maintaining communion and pastoral care while acknowledging the reality of marriage failure and human circumstance variation.
% TRANSFER_FUNCTION: Transfers interpretive authority from centralized doctrinal pronouncement to local pastoral discretion, creating space for compassionate judgment. In doing so, it transfers institutional clarity to institutional ambiguity — those seeking stable doctrine experience loss of normative certainty; those seeking mercy experience inconsistent access to it. It transfers the burden of doctrinal defense from the official magisterium to individual conscience and local bishop judgment.
% ABSENT_VOICES: Catholics who divorced and remarried entirely outside Church processes (no annulment petition, no pastoral engagement) remain structurally excluded — the discretionary mercy requires institutional contact. Conservative theologians and faithful who believe doctrinal clarity itself is a form of pastoral care would argue that the relativization of indissolubility wounds the coherence that gives the sacrament its meaning; they are present in some dioceses but excluded from the decision-making tables where synodal discernment occurs.
% DISAPPEARANCE_RATIONALE: If this reading's framework vanished — if the Church reverted to treating indissolubility as constitutive, non-negotiable doctrine with uniform enforcement — pastoral authority would revert to Rome, diocesan discretion would collapse, and the civilly remarried faithful would return to clear exclusion from communion and sacramental life. But the pastoral reading's framework is already institutionalized (synodal processes, papal language, confessor practice) — formal reversion would require active institutional will. What remains contested is whether the constraint's disappearance would restore order (traditional view) or wound the Church's credibility for pastoral sensitivity (reform view).
% FOUNDING_PROBLEM: Marriage failure is a lived reality; the Church must respond pastorally to failure while maintaining commitment to indissolubility as an ideal. The founding problem is the tension between doctrinal absolutism and pastoral responsiveness — how to honor sacramental permanence while acknowledging human frailty and the need for mercy.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral practitioners and reform theologians attest the problem is live: failed marriages remain common; compassion is a legitimate Christian virtue; people in broken remarriages suffer real spiritual exile. Vatican documents from the recent papacy acknowledge the problem explicitly. However, conservative custodians of doctrine attest that the problem is also misframed — that treating indissolubility as negotiable dissolves the very doctrine meant to uphold sacramental meaning. Independent sources (sociologists of Catholicism, historians of doctrine) confirm the tension is real; they divide on whether the pastoral reading resolves it or merely obscures it.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, contested).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__civic_pastoral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__civic_pastoral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.58 at interval end. This reflects the moderate asymmetry: pastoral authority gains discretionary interpretive power and institutional flexibility; traditional faithful lose normative clarity and institutional reliability. The constraint coordinates genuine pastoral need (responding compassionately to marriage failure) through an extractive mechanism (authority diffusion that erodes the doctrinal stability others depend on). Suppression measures 0.61: the constraint requires ongoing active maintenance — doctrinal defenders must be rhetorically minimized ('rigid' and 'juridical' versus 'merciful'), synodal processes must privilege reform voices, and formal doctrinal reaffirmation must be forestalled. Theater ratio of 0.48 reflects moderate performative maintenance: much of the constraint's persistence rides on synodal language, papal mercy rhetoric, and confessorial practice that presents itself as continuous with tradition while operationally diverging from it. Accessibility collapse is low (0.42) because the traditional Catholic position remains intellectually available — adherents can still mount coherent defenses of doctrinal clarity — and because the pastoral reading is not presented as inevitable but as one legitimate option. Resistance is high (0.71) because traditional Catholics, conservative theologians, and doctrinal custodians maintain active intellectual and institutional resistance to the reading's full normalization. The measurement series shows a rise in extractiveness, theater, and suppression from t=0 to t=24, then plateau — reflecting the stabilization of the pastoral reading as institutional practice after initial rapid adoption. By t=32 and beyond, the constraint's trajectory has settled; the projected values at t=50 show no further drift, indicating a stable but contested equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   From the pastoral authority seat, this is genuine coordination (mercy + sacramental access for the spiritually wounded) implemented through appropriate casuistry. From the traditional Catholic seat, this is the erosion of doctrinal authority and the institutional extraction of the clarity that constitutes sacramental meaning. The engine should compute different effective extraction values (high χ for traditional Catholics because they are targets of the normative shift; low/negative χ for pastoral authority because they are beneficiaries of interpretive expansion). The measurement series reflects the constraint gaining institutional weight over time — pastoral discretion becomes the de facto standard rather than an exception, which amplifies extraction for those paying the cost of instability while stabilizing benefits for those exercising discretion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (pastoral_authority_discretionary_seat): Institutional agent with highest power and exit options (arbitrage — they can choose which synodal guidance to emphasize, which confessorial practices to tolerate). Directionality low (near 0.2), effective extraction subsidizes them. Victims (traditional_identity_catholics and civilly_remarried_faithful): traditional Catholics are moderate power but identity-locked (cannot exit without abandoning the faith itself), targets of the normative shift, bearing the cost of institutional unreliability. Directionality high (near 0.85). Civilly remarried are powerless but benefit from discretionary mercy while remaining vulnerable to confessorial variation. Constrained exit (their remarriage is already accomplished; exit would require separation). Directionality mid-to-high (near 0.65). The traditional magisterium custodians are institutional but constrained by synodal momentum; their formal authority persists but their ability to enforce clarity is eroded. Directionality mid-high (near 0.72). The overrides account for the identity-lock mechanism: a moderate-power agent locked into an identity (traditional Catholicism) experiences higher effective extraction because exit requires identity dissolution, not merely changing parishes or institutions.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint has live founding problem (marriage failure is real; pastoral response is needed) and live founding problem status (these tensions persist). However, there is significant institutional contestation about whether the pastoral reading's solution resolves or obscures the founding problem. Traditional custodians argue the problem is misframed — that absolving indissolubility as ideal dissolves the doctrine entirely and abandons the very principle meant to guide pastoral response. This reading does not show the classic mandatrophy signature (founding problem dead + constraint persists), but it shows institutional mandate contestation: the pastoral reading claims to honor the founding mandate (pastoral mercy to the remarried) while critics claim it has redefined the mandate away from doctrinal clarity itself. The tangled-rope classification (coordination + extraction) prevents misreading this as pure extraction (snare): genuine pastoral coordination occurs; the extraction is the cost borne by those whose identity depends on the clarity the coordination displaces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_vs_doctrinal_clarity,
    'Does pastoral discretion in individual cases require the normative relativization of indissolubility as doctrine, or can case-by-case mercy coexist with clear doctrinal teaching?',
    'A Church that maintains formal doctrinal clarity (indissolubility is constitutive) while expanding confessorial discretion in sacramental access (some divorced-remarried receive communion on pastoral grounds) would resolve the ambiguity. Alternatively, a Church that codifies discretion as official doctrine (redefining indissolubility as ideal) would resolve in the opposite direction.',
    'If doctrinal clarity and discretional mercy are separable, the constraint''s extractiveness drops (traditional faithful retain the clarity they depend on; discretion operates as mercy within a stable framework). If inseparable, the constraint''s extractiveness remains high because any operational mercy requires doctrinal instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_doctrinal_clarity, conceptual, 'Whether pastoral discretion requires doctrinal relativization or can coexist with clear teaching.').

omega_variable(
    identity_lock_mechanism,
    'For traditional Catholics, is the experienced loss of doctrinal clarity a structural suppression (the Church has deliberately eroded clarity to exercise discretionary authority) or an internalized suppression (they have been taught to accept relativization as consistent with mercy, and now carry that frame even after doctrinal reversals)?',
    'A formal doctrinal reaffirmation of indissolubility''s constitutive status would test whether post-reversal attitudes persist (internalized) or shift back (structural). Alternatively, exit trajectories of traditional Catholics after a reversion would show whether the frame persists post-exit.',
    'Structural suppression suggests the constraint is maintained by institutional force and would collapse on formal reversal. Internalized suppression suggests the constraint''s persistence has shifted to cognitive and identity frames — the institutional authority has become less necessary because the frame is now carried by the faithful themselves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether traditional Catholic experience of doctrinal relativization is structurally or internally maintained.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the civic-pastoral reading and the hierarchical-indissolubility reading genuinely coexist within a single Catholic institutional framework, or has the civic-pastoral reading logically foreclosed the hierarchical reading (making it untenable as a live position)?',
    'Observe whether both readings are defended as legitimate within formal Church processes (synods, papal teaching, theological councils). If one is presented as inevitable or as the only coherent reading of the kernel, foreclosure is occurring. If both are defended as live options by institutionally credible seats, coexistence holds.',
    'If coexistence holds, the constraint is a stable contested equilibrium (the engine classifies coexists_with). If the civic-pastoral reading is in the process of foreclosing the hierarchical reading, the engine flags foreclosure pressure and the constraint''s long-term stability shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether these readings will coexist or whether one will foreclose the other over institutional time.').

omega_variable(
    suppression_mechanism_scope,
    'Does the suppression of doctrinal reaffirmation operate globally across all dioceses and at all institutional levels, or is it regionally variable (strong in progressive dioceses, weak in conservative ones)?',
    'Map doctrinal reaffirmation attempts (papal statements, curial documents, hierarchical teaching) and trace whether they gain local institutional traction or are undermined by diocesan-level pastoral resistance.',
    'Global suppression suggests a unified institutional constraint with top-down enforcement of discretion. Regional variability suggests multiple coexisting constraints (hierarchical in some dioceses, pastoral in others) rather than one constraint with variable suppression. The classification might shift from a single tangled rope to a network of site-specific constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_scope, empirical, 'Whether suppression of doctrinal clarity operates uniformly or is region-dependent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t8, marriage_sacrament__civic_pastoral_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(marr_tr_t8, observed).
narrative_ontology:measurement(marr_tr_t16, marriage_sacrament__civic_pastoral_reading, theater_ratio, 16, 0.43).
narrative_ontology:measurement_basis(marr_tr_t16, observed).
narrative_ontology:measurement(marr_tr_t24, marriage_sacrament__civic_pastoral_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(marr_tr_t24, observed).
narrative_ontology:measurement(marr_tr_t32, marriage_sacrament__civic_pastoral_reading, theater_ratio, 32, 0.47).
narrative_ontology:measurement_basis(marr_tr_t32, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__civic_pastoral_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(marr_tr_t40, observed).
narrative_ontology:measurement(marr_tr_t50, marriage_sacrament__civic_pastoral_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(marr_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t8, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(marr_be_t8, observed).
narrative_ontology:measurement(marr_be_t16, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement_basis(marr_be_t16, observed).
narrative_ontology:measurement(marr_be_t24, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(marr_be_t24, observed).
narrative_ontology:measurement(marr_be_t32, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement_basis(marr_be_t32, observed).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(marr_be_t40, observed).
narrative_ontology:measurement(marr_be_t50, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(marr_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t8, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(marr_su_t8, observed).
narrative_ontology:measurement(marr_su_t16, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement_basis(marr_su_t16, observed).
narrative_ontology:measurement(marr_su_t24, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement_basis(marr_su_t24, observed).
narrative_ontology:measurement(marr_su_t32, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement_basis(marr_su_t32, observed).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(marr_su_t40, observed).
narrative_ontology:measurement(marr_su_t50, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement_basis(marr_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The marriage_sacrament kernel is instantiated through two structurally distinct readings: this constraint (civic-pastoral, moderate extractiveness, pastoral discretion) and the sibling hierarchical-indissolubility reading (lower extractiveness, centralized doctrine). The readings share the referent (the Church's commitment to marriage permanence) but differ in how that commitment is epistemically and morally grounded, who has authority to interpret it, and what it mandates operationally. Neither reading provides the 'true' structure of marriage doctrine — each is a coherent instantiation of the kernel from within a different epistemic and institutional frame. The network edge indicates that changes to one reading's institutional status (e.g., formal papal reaffirmation of hierarchical indissolubility) would directly pressure the other reading's viability and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__civic_pastoral_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
