% ============================================================================
% CONSTRAINT STORY: biblical_authority__tradition_scripture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: biblical_authority__tradition_scripture_reading
 *   human_readable: Magisterial Tradition-Scripture Interpretive Authority
 *   domain: theology/religious_studies/history_of_christianity
 *
 * SUMMARY:
 *   This constraint models the reading of the biblical-authority kernel in
 *   which scripture is held to require tradition for authoritative
 *   interpretation and a magisterium is charged with guarding the deposit of
 *   faith. Under this reading, sacraments are grace-conferring acts requiring
 *   valid ordained mediation, doctrinal disputes are resolved through a
 *   centralized hierarchical apparatus rather than individual conscience or
 *   conciliar consensus alone, and lay interpretive agency is subordinated to
 *   institutionally sanctioned reading. This is a distinct constraint from
 *   the sola_scriptura_reading (which holds scripture self-sufficient and
 *   self-interpreting) and the conciliar_reading (which locates authority in
 *   ecumenical council and patristic consensus rather than an ongoing
 *   magisterial office). Each reading is authored as its own file with its
 *   own epsilon; this file's epsilon describes the tradition-scripture
 *   arrangement as it is under contest, not the alternative arrangements its
 *   sibling readings would install.
 *
 * KEY AGENTS:
 *   - ecclesial_hierarchy: institutional agenda-setter and structural beneficiary — defines licit interpretation and administers sacramental gatekeeping
 *   - ordained_clergy: organized beneficiary/local agenda-setter — vocational identity bound to mediating role
 *   - lay_interpreters: powerless payer — dependent on authorized mediation for access to grace as the arrangement defines it
 *   - vernacular_reform_movements: excluded payer — historically suppressed alternative that would dissolve the interpretive monopoly
 *   - comparative_theologians: analytical observer across all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(biblical_authority__tradition_scripture_reading, 0.68).
domain_priors:suppression_score(biblical_authority__tradition_scripture_reading, 0.62).
domain_priors:theater_ratio(biblical_authority__tradition_scripture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(biblical_authority__tradition_scripture_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(biblical_authority__tradition_scripture_reading, tangled_rope).
narrative_ontology:human_readable(biblical_authority__tradition_scripture_reading, "Magisterial Tradition-Scripture Interpretive Authority").
narrative_ontology:topic_domain(biblical_authority__tradition_scripture_reading, "theology/religious_studies/history_of_christianity").

domain_priors:requires_active_enforcement(biblical_authority__tradition_scripture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(biblical_authority__tradition_scripture_reading, 'ecc333c0-b913-4ddf-81fe-422e414e6042').
narrative_ontology:cs_kernel_codification('ecc333c0-b913-4ddf-81fe-422e414e6042', fixed_text).
narrative_ontology:cs_authority_grounding('ecc333c0-b913-4ddf-81fe-422e414e6042', lineage).
narrative_ontology:cs_interpretation_layer_present('ecc333c0-b913-4ddf-81fe-422e414e6042').
narrative_ontology:cs_reading_relation('ecc333c0-b913-4ddf-81fe-422e414e6042', biblical_authority__sola_scriptura_reading, forecloses).
narrative_ontology:cs_reading_relation('ecc333c0-b913-4ddf-81fe-422e414e6042', biblical_authority__conciliar_reading, coexists_with).
narrative_ontology:cs_axiom('ecc333c0-b913-4ddf-81fe-422e414e6042', foundational, magisterium_required_for_valid_interpretation).
narrative_ontology:cs_axiom_status(magisterium_required_for_valid_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('ecc333c0-b913-4ddf-81fe-422e414e6042', magisterium_required_for_valid_interpretation, conventional).
narrative_ontology:cs_axiom('ecc333c0-b913-4ddf-81fe-422e414e6042', foundational, sacraments_confer_grace_ex_opere_operato_via_valid_ordination).
narrative_ontology:cs_axiom_status(sacraments_confer_grace_ex_opere_operato_via_valid_ordination, holdable).
narrative_ontology:cs_axiom_grounding('ecc333c0-b913-4ddf-81fe-422e414e6042', sacraments_confer_grace_ex_opere_operato_via_valid_ordination, theological).
narrative_ontology:cs_reference_frame('ecc333c0-b913-4ddf-81fe-422e414e6042', apostolic_deposit_continuity).
narrative_ontology:cs_drift_state('ecc333c0-b913-4ddf-81fe-422e414e6042', post_reformation_challenge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ecc333c0-b913-4ddf-81fe-422e414e6042', '').
narrative_ontology:cs_kernel_id(biblical_authority__tradition_scripture_reading, biblical_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, ecclesial_hierarchy).
narrative_ontology:constraint_beneficiary(biblical_authority__tradition_scripture_reading, ordained_clergy).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, lay_interpreters).
narrative_ontology:constraint_victim(biblical_authority__tradition_scripture_reading, vernacular_reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines and promulgates the magisterium's authority to adjudicate doctrine, declares which readings of scripture are licit, and administers the sacramental system through which grace is understood to be conferred. Sets canon law governing who may teach and interpret, and holds disciplinary power (excommunication, censure) over dissenting readings. Its own institutional continuity is the deposit it claims to guard.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, ecclesial_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, ecclesial_hierarchy, beneficiary).

% Administer sacraments and preach authorized interpretation locally, deriving vocational status, income, and social authority from being the necessary conduit between scripture, tradition, and the laity. Career and identity are bound to the hierarchy's continued claim that mediation is required for valid grace; leaving the office forfeits standing built over a lifetime.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, ordained_clergy, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, ordained_clergy, agenda_setter).

% Depend on clergy and magisterial teaching to receive authorized readings of scripture; independent interpretation outside sanctioned channels risks charges of heresy or informal social exclusion from the sacramental community. Access to grace, as the arrangement defines it, is routed through the very authority whose interpretive monopoly they cannot independently verify or contest.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, lay_interpreters, payer,
    powerless, biographical, constrained, local).

% Advocate direct lay access to scripture in vernacular language without mandatory magisterial mediation. Historically met with suppression (translation bans, trial, execution in some eras) because their success would dissolve the interpretive monopoly the arrangement depends on; where tolerated, they remain marginalized within the sanctioned communion.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, vernacular_reform_movements, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(biblical_authority__tradition_scripture_reading, vernacular_reform_movements, excluded).

% The body of inherited commentary and doctrinal formulation that the magisterium cites as corroborating evidence for its interpretive authority. Not an actor itself, but its invocation legitimizes the hierarchy's claim that scripture cannot be read authoritatively in isolation from accumulated tradition.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, patristic_textual_tradition, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(biblical_authority__tradition_scripture_reading, patristic_textual_tradition).

% Study the historical development of magisterial authority claims, compare them against sola scriptura and conciliar accounts, and assess whether the tradition-plus-scripture model reflects apostolic continuity or later institutional consolidation. Take testimony from all traditions without needing to submit to any one hierarchy's discipline.
narrative_ontology:constraint_stakeholder(biblical_authority__tradition_scripture_reading, comparative_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(biblical_authority__tradition_scripture_reading, ecclesial_hierarchy).
narrative_ontology:fixing_cost_class(biblical_authority__tradition_scripture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, stable, non-fragmenting mechanism for resolving disputed scriptural interpretation across a geographically and culturally dispersed body of believers, preventing the doctrinal chaos of unmediated individual reading and preserving continuity of practice across centuries.
% TRANSFER_FUNCTION: Moves interpretive authority, sacramental gatekeeping power, and the associated social and material resources (tithes, vocational status, disciplinary reach) from individual believers to the ordained hierarchy, in exchange for doctrinal stability and claimed assurance of valid grace.
% ABSENT_VOICES: Vernacular reform movements and independent lay exegetes are structurally excluded from authorized interpretation; historically their translations were banned and their teachers tried. They would argue scripture is sufficiently perspicuous for the literate believer and that mediation is unnecessary superstructure, but their position is foreclosed by the very doctrine under evaluation.
% DISAPPEARANCE_RATIONALE: If magisterial mediation vanished overnight, sacramental economies built on clerical exclusivity would collapse, doctrinal authority would fragment into competing lay and conciliar readings (as happened historically wherever the claim was successfully rejected), and the material and social structures dependent on clerical office would need to reorganize around some other legitimating mechanism.
% FOUNDING_PROBLEM: Early Christian communities faced genuine interpretive chaos and doctrinal fragmentation (gnostic variants, competing canons, christological disputes) without a stable mechanism to adjudicate which readings were authoritative; a centralized interpretive authority answered a live coordination problem in a dispersed, persecuted, largely illiterate movement.
% FOUNDING_PROBLEM_CORROBORATION: The hierarchy itself attests the problem remains live, citing ongoing doctrinal controversy as evidence mediation is still necessary. Historians of early Christianity and comparative theologians outside the hierarchy corroborate that the founding coordination problem (fragmentation under persecution and low literacy) was real but note it was substantially resolved by late antiquity; the reform movements and independent scholarship argue the mediating apparatus has persisted well past the conditions (illiteracy, absence of vernacular text, dispersion without communication infrastructure) that made it necessary.
narrative_ontology:disappearance_verdict(biblical_authority__tradition_scripture_reading, world_rearranges).
narrative_ontology:founding_problem_status(biblical_authority__tradition_scripture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(biblical_authority__tradition_scripture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(biblical_authority__tradition_scripture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(biblical_authority__tradition_scripture_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high-moderate (0.68 at interval end) because the arrangement makes access to sacramental grace structurally contingent on paying (materially and socially) for clerical mediation that this reading holds to be theologically necessary rather than merely administratively convenient. Suppression peaked mid-interval (~0.78 around the era of vernacular Bible bans and associated trials) before declining somewhat as translation tolerance widened in later centuries, though it remains substantial (0.62) because unauthorized independent interpretation still carries doctrinal and communal consequences. Theater ratio rose over the interval (0.15 to 0.42) reflecting the accumulation of increasingly elaborate procedural and juridical apparatus around doctrinal adjudication relative to the coordination function it nominally serves. Accessibility collapse is authored at 0.6, reflecting that alternatives (direct lay reading, conciliar consensus without ongoing magisterial office) persisted as live options historically and persist today as sibling readings, rather than being erased entirely — this is not a mountain-grade collapse.
 *
 * PERSPECTIVAL GAP:
 *   From the hierarchy's own seat, the arrangement is experienced as faithful stewardship of a genuine deposit against fragmentation and heresy. From the lay_interpreters' seat, the same structure is experienced as a gate that must be paid to pass through for what the tradition itself defines as necessary for salvation. The engine computes these as different seat-level classifications from the same structural data; the claimed_type (tangled_rope) is authored as the generating model's independent judgment, not a reconciliation of the two seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The ecclesial hierarchy and ordained clergy sit near the beneficiary end of directionality: they set the interpretive rules, administer the sacramental gate, and derive vocational and institutional continuity from the arrangement's persistence. Lay interpreters sit near the target end: they bear the cost of interpretive dependency without independent means to verify or contest the authorized reading, and their exit options are constrained by the arrangement's own definition of what counts as valid access to grace. Vernacular reform movements sit at the extreme target end (trapped exit) precisely because their success is what the enforcement machinery (translation bans, heresy trials) exists to prevent — they are not merely under-resourced dissenters but the structurally excluded alternative reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (doctrinal chaos and fragmentation across a dispersed, persecuted, largely illiterate early church) was genuinely live for centuries, which is why this reading is authored as tangled_rope rather than pure snare — there is a real coordination function underneath the extraction. But the founding_problem_status is authored contested because the conditions that made centralized adjudication necessary (illiteracy, absence of vernacular scripture, lack of communication infrastructure across dispersed communities) have substantially changed, while the mediating apparatus and its extractive features have not correspondingly receded — this is the signature the classification is designed to surface rather than average away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    magisterium_reading_of_kernel,
    'Is the tradition-scripture reading of biblical authority a faithful continuation of apostolic practice, or a later institutional consolidation that retrofits doctrinal necessity onto what began as administrative convenience?',
    'This is the committer-level ambiguity the kernel decomposition exists to hold open: a sibling reading (conciliar_reading) locates authority in councils and patristic consensus without an ongoing magisterial office, and another (sola_scriptura_reading) denies mediation is necessary at all. Historical-critical scholarship on the development of the monarchical episcopate and canon formation bears on this but does not settle it, since the readings differ in what counts as valid evidence of continuity.',
    'If the magisterial reading is judged a later consolidation rather than apostolic continuity, the coordination-function claim underlying the tangled_rope classification weakens toward pure extraction (snare); if judged genuine continuity, the coordination function is stronger and the classification is more securely tangled_rope rather than snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterium_reading_of_kernel, conceptual, 'Whether this reading''s magisterial authority claim is continuation or retrofit — the core committer ambiguity.').

omega_variable(
    sacramental_grace_beneficiary_structure,
    'Is the sacramental system a genuine channel of grace requiring valid ordination (in which case clergy are conduits, not beneficiaries, of a good they do not themselves generate), or is the requirement of mediation itself the extractive mechanism dressed in theological necessity?',
    'Irreducible: this question is theological, not empirical, and different traditions within the same kernel contest give structurally incompatible answers. No external corroboration is available because the claim (valid grace requires valid mediation) is internal to the framework being evaluated.',
    'If sacramental necessity is theologically real, clerical benefit is incidental to a genuine coordination good; if the necessity claim is itself the extractive move, clergy and hierarchy are beneficiaries of a manufactured scarcity (access to grace) rather than administrators of a pre-existing one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_grace_beneficiary_structure, preference, 'Whether sacramental mediation is a genuine grace-channel or a manufactured scarcity justifying clerical benefit.').

omega_variable(
    vernacular_suppression_necessity,
    'Was historical suppression of vernacular scripture and independent lay interpretation a proportionate response to a genuine risk of doctrinal chaos, or was doctrinal-chaos risk the cover story for protecting the interpretive monopoly''s material and social benefits?',
    'Comparative historical analysis of regions/periods where vernacular access was tolerated versus suppressed, examining whether tolerance correlated with measurable doctrinal instability or primarily with loss of institutional revenue and authority.',
    'If suppression tracked genuine instability risk, the high historical suppression_requirement measurements reflect defensive coordination cost; if it tracked institutional revenue protection, the same measurements reflect pure extraction enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_suppression_necessity, empirical, 'Whether historical suppression of vernacular scripture was risk-proportionate or revenue-protective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(biblical_authority__tradition_scripture_reading, 0, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bibl_tr_t0, biblical_authority__tradition_scripture_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(bibl_tr_t300, biblical_authority__tradition_scripture_reading, theater_ratio, 300, 0.2).
narrative_ontology:measurement(bibl_tr_t700, biblical_authority__tradition_scripture_reading, theater_ratio, 700, 0.3).
narrative_ontology:measurement(bibl_tr_t1000, biblical_authority__tradition_scripture_reading, theater_ratio, 1000, 0.38).
narrative_ontology:measurement(bibl_tr_t1300, biblical_authority__tradition_scripture_reading, theater_ratio, 1300, 0.42).
narrative_ontology:measurement(bibl_tr_t1600, biblical_authority__tradition_scripture_reading, theater_ratio, 1600, 0.4).

% Extraction over time
narrative_ontology:measurement(bibl_be_t0, biblical_authority__tradition_scripture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bibl_be_t300, biblical_authority__tradition_scripture_reading, base_extractiveness, 300, 0.45).
narrative_ontology:measurement(bibl_be_t700, biblical_authority__tradition_scripture_reading, base_extractiveness, 700, 0.58).
narrative_ontology:measurement(bibl_be_t1000, biblical_authority__tradition_scripture_reading, base_extractiveness, 1000, 0.66).
narrative_ontology:measurement(bibl_be_t1300, biblical_authority__tradition_scripture_reading, base_extractiveness, 1300, 0.7).
narrative_ontology:measurement(bibl_be_t1600, biblical_authority__tradition_scripture_reading, base_extractiveness, 1600, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bibl_su_t0, biblical_authority__tradition_scripture_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(bibl_su_t300, biblical_authority__tradition_scripture_reading, suppression_requirement, 300, 0.4).
narrative_ontology:measurement(bibl_su_t700, biblical_authority__tradition_scripture_reading, suppression_requirement, 700, 0.55).
narrative_ontology:measurement(bibl_su_t1000, biblical_authority__tradition_scripture_reading, suppression_requirement, 1000, 0.7).
narrative_ontology:measurement(bibl_su_t1300, biblical_authority__tradition_scripture_reading, suppression_requirement, 1300, 0.78).
narrative_ontology:measurement(bibl_su_t1600, biblical_authority__tradition_scripture_reading, suppression_requirement, 1600, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(biblical_authority__tradition_scripture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(biblical_authority__tradition_scripture_reading, 0.12).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, sola_scriptura_reading).
narrative_ontology:affects_constraint(biblical_authority__tradition_scripture_reading, conciliar_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the biblical_authority kernel. sola_scriptura_reading claims scripture is self-sufficient and self-interpreting (low clerical extraction, high doctrinal fragmentation risk, no magisterial beneficiary). conciliar_reading locates authority in councils and patristic consensus as living continuity rather than an ongoing magisterial office (moderate extraction, distributed rather than centralized beneficiary). This tradition_scripture_reading claims magisterial mediation is required (high clerical extraction, low fragmentation, centralized institutional beneficiary, lay interpretive agency as victim). Each carries its own epsilon and stakeholder set per the epsilon-invariance principle; they are linked here rather than merged because they instantiate structurally distinct constraints from the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(biblical_authority__tradition_scripture_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
