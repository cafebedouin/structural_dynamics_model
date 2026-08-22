% ============================================================================
% CONSTRAINT STORY: vatican_ii_authority__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_authority__rupture_reading, []).

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
 *   constraint_id: vatican_ii_authority__rupture_reading
 *   human_readable: Vatican II as Rupture — Council Invalid/Gravely Defective Reading
 *   domain: theology/ecclesiology/religious_authority
 *
 * SUMMARY:
 *   This story instantiates the rupture reading of the contested Vatican II
 *   authority kernel: the claim that the Council's documents on religious
 *   liberty (Dignitatis Humanae), ecumenism (Unitatis Redintegratio), and
 *   collegiality (Lumen Gentium) contain doctrinal errors or are
 *   irreconcilable with prior irreformable magisterial teaching, rendering
 *   the Council itself invalid or gravely defective in its authority. This is
 *   a distinct constraint from the continuity_reading (organic development,
 *   no contradiction) and the composite_overdetermination_reading (structural
 *   ambiguity resolvable into neither continuity nor rupture) — the three are
 *   siblings in one kernel, not three measurements of one constraint. The
 *   rupture reading's own referent for extraction is the post-conciliar
 *   arrangement AS THIS READING SEES IT: an institutional apparatus that
 *   suppresses and marginalizes the discontinuity claim rather than
 *   adjudicating it, extracting institutional conformity from traditional
 *   Catholics under a claim of unbroken continuity the reading holds to be
 *   false.
 *
 * KEY AGENTS:
 *   - post_conciliar_curial_establishment: institutional agenda-setter administering the post-1965 doctrinal and liturgical regime
 *   - modernist_theological_faction: organized beneficiary whose academic and institutional standing depends on the conciliar reforms being read as legitimate development
 *   - traditional_catholic_laity: powerless payer whose inherited catechesis and sacramental practice were reorganized without consent
 *   - traditionalist_clergy_and_religious: moderate-power payer facing canonical risk for maintaining the discontinuity claim
 *   - sspx_and_affiliated_traditionalist_bodies: organized excluded party that institutionalized the rupture reading into a durable but canonically irregular structure
 *   - magisterial_historians_and_canonists: analytical observer documenting textual and drafting ambiguity without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, 0.62).
domain_priors:suppression_score(vatican_ii_authority__rupture_reading, 0.71).
domain_priors:theater_ratio(vatican_ii_authority__rupture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vatican_ii_authority__rupture_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_authority__rupture_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_authority__rupture_reading, "Vatican II as Rupture — Council Invalid/Gravely Defective Reading").
narrative_ontology:topic_domain(vatican_ii_authority__rupture_reading, "theology/ecclesiology/religious_authority").

domain_priors:requires_active_enforcement(vatican_ii_authority__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_authority__rupture_reading, 'b5207bd8-e540-45a4-ab25-7490d1024a99').
narrative_ontology:cs_kernel_codification('b5207bd8-e540-45a4-ab25-7490d1024a99', fixed_text).
narrative_ontology:cs_authority_grounding('b5207bd8-e540-45a4-ab25-7490d1024a99', lineage).
narrative_ontology:cs_interpretation_layer_present('b5207bd8-e540-45a4-ab25-7490d1024a99').
narrative_ontology:cs_reading_relation('b5207bd8-e540-45a4-ab25-7490d1024a99', vatican_ii_authority__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('b5207bd8-e540-45a4-ab25-7490d1024a99', vatican_ii_authority__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('b5207bd8-e540-45a4-ab25-7490d1024a99', foundational, prior_irreformable_teaching_binds_absolutely).
narrative_ontology:cs_axiom_status(prior_irreformable_teaching_binds_absolutely, holdable).
narrative_ontology:cs_axiom_grounding('b5207bd8-e540-45a4-ab25-7490d1024a99', prior_irreformable_teaching_binds_absolutely, deontological).
narrative_ontology:cs_axiom('b5207bd8-e540-45a4-ab25-7490d1024a99', foundational, conciliar_texts_contain_genuine_doctrinal_contradiction).
narrative_ontology:cs_axiom_status(conciliar_texts_contain_genuine_doctrinal_contradiction, holdable).
narrative_ontology:cs_axiom_grounding('b5207bd8-e540-45a4-ab25-7490d1024a99', conciliar_texts_contain_genuine_doctrinal_contradiction, empirically_contingent).
narrative_ontology:cs_reference_frame('b5207bd8-e540-45a4-ab25-7490d1024a99', pre_conciliar_magisterial_settlement).
narrative_ontology:cs_drift_state('b5207bd8-e540-45a4-ab25-7490d1024a99', post_conciliar_contemporary, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('b5207bd8-e540-45a4-ab25-7490d1024a99', '').
narrative_ontology:cs_kernel_id(vatican_ii_authority__rupture_reading, vatican_ii_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, modernist_theological_faction).
narrative_ontology:constraint_beneficiary(vatican_ii_authority__rupture_reading, post_conciliar_curial_establishment).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditional_catholic_laity).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, traditionalist_clergy_and_religious).
narrative_ontology:constraint_victim(vatican_ii_authority__rupture_reading, doctrinal_continuity_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the post-1965 liturgical, ecumenical, and doctrinal apparatus (Novus Ordo Mass, revised catechetical formulas, ecumenical dialogue structures) and enforces conformity through episcopal appointment, seminary formation, and canonical discipline against dissenting clergy. From the rupture reading's vantage, this establishment collects institutional continuity and legitimacy precisely by suppressing the claim that the Council broke with prior teaching.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, post_conciliar_curial_establishment, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, post_conciliar_curial_establishment, beneficiary).

% Academic theologians and periti whose post-conciliar careers, publications, and institutional standing rest on reading the Council's documents on religious liberty, ecumenism, and collegiality as genuine doctrinal advances. The rupture reading holds that this faction's professional and reputational capital depends on the discontinuity being real but denied.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, modernist_theological_faction, beneficiary,
    organized, generational, mobile, global).

% Ordinary Catholics formed in pre-conciliar catechesis who experienced the liturgical and doctrinal changes as a rupture in what they were taught was unchangeable. Their parishes, sacramental access, and catechetical formation were reorganized around the new documents without their consent; leaving means either accepting the new regime, joining an irregular chapel, or leaving the Church entirely.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditional_catholic_laity, payer,
    powerless, biographical, constrained, global).

% Priests, religious, and seminarians who hold that the Council's documents are gravely defective or irreconcilable with prior magisterial teaching. Continuing to say so publicly risks canonical penalty, suspension, or expulsion from religious institutes; the SSPX and sedevacantist positions represent the exit chosen by those who judged the cost of staying too high.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, traditionalist_clergy_and_religious, payer,
    moderate, biographical, trapped, global).

% Theologians and canonists who hold that the pre-conciliar magisterium is binding and irreformable on the specific points (religious liberty, ecumenism, collegiality) the rupture reading identifies as contradicted. Their claim that doctrinal stability itself is being sacrificed to institutional accommodation is treated as fringe or schismatic by the post-conciliar establishment rather than engaged on the merits.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, doctrinal_continuity_claimants, payer,
    moderate, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(vatican_ii_authority__rupture_reading, doctrinal_continuity_claimants, excluded).

% Institutionalized the rupture reading into a durable ecclesial structure (the Society of St. Pius X and similar bodies), operating in canonically irregular or contested status precisely because they refuse to accept the disputed conciliar teachings as binding. They are excluded from ordinary channels of magisterial dialogue and treated as a discipline problem rather than as bearers of a live theological claim.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, sspx_and_affiliated_traditionalist_bodies, excluded,
    organized, generational, trapped, global).

% Study conciliar text formation, drafting history, and reception without institutional stake in either outcome. They document the textual ambiguities and drafting compromises that both continuity and rupture readings cite as evidence for their own position.
narrative_ontology:constraint_stakeholder(vatican_ii_authority__rupture_reading, magisterial_historians_and_canonists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vatican_ii_authority__rupture_reading, diffuse).
narrative_ontology:fixing_cost_class(vatican_ii_authority__rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To the extent any coordination exists in this reading's own account, it is negative: the rupture reading exists to coordinate resistance among those who judge the post-conciliar regime doctrinally compromised, preserving a pre-1962 doctrinal and liturgical identity against what it sees as unauthorized substantive change.
% TRANSFER_FUNCTION: Institutional legitimacy, sacramental normalcy, and catechetical authority are transferred from the pre-conciliar magisterial settlement to the post-conciliar administrative apparatus; those unwilling to accept the transfer bear the cost of irregular canonical status, social marginalization within mainstream Catholic institutions, or exit.
% ABSENT_VOICES: The rupture reading's clergy and canonists are largely excluded from official magisterial dialogue — treated as a disciplinary matter (SSPX canonical status) rather than invited to argue the doctrinal-contradiction claim in a forum with binding authority to adjudicate it.
% DISAPPEARANCE_RATIONALE: If the rupture reading vanished overnight (i.e., if no one held it), the SSPX and sedevacantist structures would lose their reason for existing and traditionalist Catholic identity as a distinct contested category would dissolve into either full conformity or a different objection; the post-conciliar establishment would lose its primary internal doctrinal challenger. Whether this counts as the world 'rearranging' or 'unchanging' is itself disputed between the reading's adherents (who see themselves as the last barrier against doctrinal collapse) and the establishment (which sees them as a marginal irritant whose disappearance would change little).
% FOUNDING_PROBLEM: The rupture reading was built to name and resist what its adherents perceive as a discontinuity between conciliar teaching (on religious liberty, ecumenism, collegiality, and the liturgy) and prior irreformable magisterial teaching — a problem of doctrinal identity and continuity, not merely of pastoral style.
% FOUNDING_PROBLEM_CORROBORATION: Adherents (SSPX, sedevacantist writers, traditionalist canonists) attest the discontinuity is real and unresolved. Mainstream magisterial authorities and most professional theologians attest the problem is dissolved by a correct hermeneutic of continuity. Independent historians of the Council's drafting process (a source outside both benefiting camps) corroborate that genuine textual ambiguity and compromise language exist in several conciliar documents, which is evidence for the existence of a live interpretive problem without adjudicating which reading resolves it correctly.
narrative_ontology:disappearance_verdict(vatican_ii_authority__rupture_reading, contested).
narrative_ontology:founding_problem_status(vatican_ii_authority__rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vatican_ii_authority__rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vatican_ii_authority__rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vatican_ii_authority__rupture_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_authority__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_authority__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vatican_ii_authority__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects this reading's assessment that institutional legitimacy, catechetical authority, and sacramental normalcy were transferred from the pre-conciliar settlement to the post-conciliar apparatus without resolving (indeed while suppressing) the discontinuity claim — a real transfer imposed on those who did not accept its premises. Suppression (0.71) is high and reflects active canonical and institutional pressure against traditionalist clergy (culminating structurally in events like the 1988 SSPX episcopal consecrations and subsequent excommunications, reflected in the suppression_requirement peak at 1988) rather than mere disagreement. Theater ratio (0.40) reflects a real but partial performative element: ecumenical and reform activity that this reading holds continues to be defended rhetorically as 'continuity' despite what it identifies as substantive change. Accessibility collapse is moderate (0.50) — the pre-conciliar liturgical and doctrinal alternative was not eliminated outright (indult and later Ecclesia Dei / Summorum Pontificum provisions preserved partial access) but was marginalized and made administratively difficult to access for decades. Resistance is high (0.78): this reading exists BECAUSE of sustained, organized resistance (SSPX, sedevacantist movements, individual clergy discipline cases) — a genuine natural law would meet negligible resistance; this constraint meets substantial and organized resistance, which is itself diagnostic against a 'mountain' framing and consistent with the tangled_rope claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-conciliar curial establishment and modernist theological faction sit near the beneficiary end: they administer and collect institutional legitimacy from the arrangement the rupture reading contests. Traditional Catholic laity, traditionalist clergy, and doctrinal continuity claimants sit near the target end: they bear the cost of a transfer they did not request and whose validity they dispute, with constrained-to-trapped exit options (leaving means schism, canonical irregularity, or exiting the institution as such). The SSPX is deliberately coded as excluded rather than merely payer, because its structural position is one of exclusion from the adjudicating forum, not merely cost-bearing within it — a point the coordination_function and absent_voices answers make explicit.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than pure snare) reflects that this reading does not deny the Council solved SOME genuine coordination problem for SOME actors — it holds that the post-conciliar establishment coordinates its own legitimacy and institutional continuity around the disputed documents, which is a real (if, on this reading, illegitimately grounded) coordination function; the extraction is that this coordination function is purchased at the price of traditional Catholic doctrinal identity and stability, imposed via active enforcement (canonical discipline, seminary formation control, liturgical mandate) rather than by voluntary consensus. This prevents mislabeling the conflict as pure top-down extraction with no coordination function at all (which would ignore that most Catholics under this regime experience it as ordinary, functioning church life) while also refusing to launder the arrangement as costless 'organic development' (which is precisely what the sibling continuity_reading claims and this reading denies).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conciliar_documents_binding_authority_level,
    'Are the specific conciliar teachings this reading identifies as contradictory (religious liberty, ecumenism, collegiality) infallible/irreformable magisterial acts, or non-infallible pastoral/prudential teaching subject to revision — and does the answer differ document by document?',
    'A definitive, binding magisterial ruling on the theological note (level of authority) of each contested passage — something that has not been issued and may be structurally unlikely to be issued given the stakes for all sides.',
    'If the contested passages are non-infallible, the ''doctrinal error'' claim central to this reading loses its strongest form (development/change without infallibility is not automatically contradiction); if genuinely irreformable, the rupture reading''s core claim is strengthened toward requiring resolution of an actual contradiction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conciliar_documents_binding_authority_level, conceptual, 'Whether the contested conciliar passages carry the level of authority the rupture reading''s contradiction claim requires.').

omega_variable(
    kernel_reading_committer_structure,
    'This story is one reading (rupture_reading) of the vatican_ii_authority kernel; the sibling readings (continuity_reading, composite_overdetermination_reading) hold that the discontinuity is either illusory (organic development) or structurally unresolvable (overdetermined composite) rather than a genuine invalidating contradiction. Which structural element are the readings actually disagreeing about?',
    'A comparative textual-historical analysis of conciliar drafting history (relatio, schema revisions, minority/majority position papers) could establish whether the disputed passages represent (a) genuine doctrinal reversal, (b) legitimate development, or (c) irreducible compromise language admitting multiple readings — this is exactly the kind of empirical-textual question a documented drafting record can partially adjudicate, though the normative question of which reading is theologically correct remains contested regardless.',
    'If drafting-history analysis strongly supports compromise/ambiguity (the composite_overdetermination_reading''s core claim), this reading''s confident ''invalid or gravely defective'' framing is weakened relative to a milder ''genuinely ambiguous, contested in good faith'' framing; if it supports a clean discontinuity from prior binding teaching, this reading''s core claim is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Locates the actual site of disagreement between the three sibling kernel readings in the drafting-history record.').

omega_variable(
    beneficiary_capture_vs_genuine_belief,
    'Is the post-conciliar establishment''s rejection of the rupture reading driven by genuine theological conviction that continuity holds, or by institutional self-interest in not admitting a crisis that would undermine six decades of accumulated legitimacy?',
    'No clean empirical test exists; would require examining whether establishment figures who privately doubt continuity nonetheless publicly enforce it, which is not observable at scale.',
    'If institutional self-interest dominates, the tangled_rope''s ''active enforcement'' element is better characterized as protecting a beneficiary faction''s position rather than defending a sincerely-held theological settlement — sharpening the extraction reading. If genuine conviction dominates, the enforcement is better read as ordinary doctrinal discipline rather than extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_genuine_belief, preference, 'Whether establishment resistance to the rupture reading reflects sincere belief or institutional self-protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_authority__rupture_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vati_tr_t1965, vatican_ii_authority__rupture_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(vati_tr_t1975, vatican_ii_authority__rupture_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(vati_tr_t1988, vatican_ii_authority__rupture_reading, theater_ratio, 1988, 0.34).
narrative_ontology:measurement(vati_tr_t2000, vatican_ii_authority__rupture_reading, theater_ratio, 2000, 0.36).
narrative_ontology:measurement(vati_tr_t2013, vatican_ii_authority__rupture_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(vati_tr_t2025, vatican_ii_authority__rupture_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(vati_be_t1965, vatican_ii_authority__rupture_reading, base_extractiveness, 1965, 0.35).
narrative_ontology:measurement(vati_be_t1975, vatican_ii_authority__rupture_reading, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(vati_be_t1988, vatican_ii_authority__rupture_reading, base_extractiveness, 1988, 0.55).
narrative_ontology:measurement(vati_be_t2000, vatican_ii_authority__rupture_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(vati_be_t2013, vatican_ii_authority__rupture_reading, base_extractiveness, 2013, 0.6).
narrative_ontology:measurement(vati_be_t2025, vatican_ii_authority__rupture_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vati_su_t1965, vatican_ii_authority__rupture_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(vati_su_t1975, vatican_ii_authority__rupture_reading, suppression_requirement, 1975, 0.6).
narrative_ontology:measurement(vati_su_t1988, vatican_ii_authority__rupture_reading, suppression_requirement, 1988, 0.75).
narrative_ontology:measurement(vati_su_t2000, vatican_ii_authority__rupture_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(vati_su_t2013, vatican_ii_authority__rupture_reading, suppression_requirement, 2013, 0.68).
narrative_ontology:measurement(vati_su_t2025, vatican_ii_authority__rupture_reading, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_authority__rupture_reading, vatican_ii_authority__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint stories decomposing the natural-language label 'Vatican II authority' per the epsilon-invariance principle. continuity_reading authors low extraction (organic development, no genuine contradiction); rupture_reading (this story) authors substantial extraction (invalid/gravely defective council, active suppression of the discontinuity claim); composite_overdetermination_reading authors a distinct structural claim (irreducible ambiguity, not resolvable into either pole). All three share the same kernel_id (vatican_ii_authority) and are linked bidirectionally via affects_constraints. Do not average their ε values or treat them as three measurements of one constraint — each is a structurally distinct claim with its own beneficiary/victim set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
