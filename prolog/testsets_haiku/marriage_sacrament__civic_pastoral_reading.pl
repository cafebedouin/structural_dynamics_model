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
 *   human_readable: Marriage as Pastoral Relationship (Civic-Pastoral Reading)
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   This constraint represents one coherent reading of the contested
 *   sacrament-of-marriage kernel: marriage as a pastoral relationship subject
 *   to human failure, with indissolubility reframed as an ideal
 *   (aspirational) rather than a constitutive metaphysical reality. Under
 *   this reading, the Church's authority shifts from centralized hierarchical
 *   judgment (Rome decides annulment cases on metaphysical grounds) to
 *   distributed pastoral discernment (bishops and conscience formation admit
 *   remarried persons to communion). The constraint is CLAIMED as
 *   tangled_rope because it genuinely coordinates a resolution to pastoral
 *   crisis (remarried Catholics re-integrated into communion; pastoral mercy
 *   vindicated) while simultaneously extracting authority costs from
 *   traditional Catholics who lose doctrinal certainty and normative
 *   coherence. The claim/metric divergence is deliberate: traditional
 *   Catholics experience this constraint as extractive precisely because
 *   coordination and extraction are structurally intertwined — the mercy for
 *   some is the confusion for others. The sibling reading
 *   (hierarchical_indissolubility_reading) represents the alternative
 *   coherent framing of the same kernel: marriage as ontologically
 *   indissoluble, requiring centralized adjudication, indissolubility as
 *   constitutive fact not pastoral aspiration. These are not
 *   observer-relative interpretations of a neutral fact; they are
 *   incompatible commitments about what marriage IS and WHO adjudicates it.
 *
 * KEY AGENTS:
 *   - Pastoral bishops and synodal clergy: institutional actors gaining discretionary authority over marriage adjudication and sacramental admission.
 *   - Remarried divorced Catholics: organized beneficiaries regaining sacramental participation and pastoral recognition.
 *   - Traditional Catholics identity-locked: moderate-power victims experiencing doctrinal relativization and loss of institutional coherence.
 *   - Laity seeking clarity: powerless victims bearing the cost of inconsistent enforcement and ambiguous guidance.
 *   - Roman Curia: retaining doctrinal supremacy while operationally accepting variation — dual-position agenda-setter.
 *   - Hierarchical indissolubility readers: excluded from this reading's problem construction, positioned as rigorist opponents.
 *   - Doctrinal theologians: observers providing interpretive infrastructure (novel concepts: psychological maturity, sacramental intention) that reframe old doctrine as newly intelligible.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__civic_pastoral_reading, 0.62).
domain_priors:suppression_score(marriage_sacrament__civic_pastoral_reading, 0.71).
domain_priors:theater_ratio(marriage_sacrament__civic_pastoral_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_sacrament__civic_pastoral_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__civic_pastoral_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__civic_pastoral_reading, "Marriage as Pastoral Relationship (Civic-Pastoral Reading)").
narrative_ontology:topic_domain(marriage_sacrament__civic_pastoral_reading, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(marriage_sacrament__civic_pastoral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__civic_pastoral_reading, '6040f10e-86bf-40b2-9e58-a3401a3d13ee').
narrative_ontology:cs_kernel_codification('6040f10e-86bf-40b2-9e58-a3401a3d13ee', fixed_text).
narrative_ontology:cs_authority_grounding('6040f10e-86bf-40b2-9e58-a3401a3d13ee', lineage).
narrative_ontology:cs_interpretation_layer_present('6040f10e-86bf-40b2-9e58-a3401a3d13ee').
narrative_ontology:cs_reading_relation('6040f10e-86bf-40b2-9e58-a3401a3d13ee', marriage_sacrament__hierarchical_indissolubility_reading, coexists_with).
narrative_ontology:cs_axiom('6040f10e-86bf-40b2-9e58-a3401a3d13ee', foundational, marriage_as_relational_reality_subject_to_failure).
narrative_ontology:cs_axiom_status(marriage_as_relational_reality_subject_to_failure, holdable).
narrative_ontology:cs_axiom_grounding('6040f10e-86bf-40b2-9e58-a3401a3d13ee', marriage_as_relational_reality_subject_to_failure, deontological).
narrative_ontology:cs_axiom('6040f10e-86bf-40b2-9e58-a3401a3d13ee', foundational, indissolubility_as_aspirational_ideal_not_constitutive_necessity).
narrative_ontology:cs_axiom_status(indissolubility_as_aspirational_ideal_not_constitutive_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6040f10e-86bf-40b2-9e58-a3401a3d13ee', indissolubility_as_aspirational_ideal_not_constitutive_necessity, deontological).
narrative_ontology:cs_axiom('6040f10e-86bf-40b2-9e58-a3401a3d13ee', secondary, pastoral_mercy_precedence_over_metaphysical_claim).
narrative_ontology:cs_axiom_status(pastoral_mercy_precedence_over_metaphysical_claim, holdable).
narrative_ontology:cs_axiom_grounding('6040f10e-86bf-40b2-9e58-a3401a3d13ee', pastoral_mercy_precedence_over_metaphysical_claim, deontological).
narrative_ontology:cs_reference_frame('6040f10e-86bf-40b2-9e58-a3401a3d13ee', vatican_ii_pastoral_framework).
narrative_ontology:cs_drift_state('6040f10e-86bf-40b2-9e58-a3401a3d13ee', contemporary_post_amoris_laetitia_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6040f10e-86bf-40b2-9e58-a3401a3d13ee', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__civic_pastoral_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, pastoral_bishops_and_synodal_clergy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__civic_pastoral_reading, remarried_divorced_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, traditional_catholics_identity_locked).
narrative_ontology:constraint_victim(marriage_sacrament__civic_pastoral_reading, laity_seeking_doctrinal_clarity).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, mercy_over_rigor_doctrine).
narrative_ontology:constraint_vindicates(marriage_sacrament__civic_pastoral_reading, pastoral_discernment_authority_distributed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and adjudicate marriage cases on grounds of pastoral mercy and human limitation. They gain discretionary authority (previously held at Rome) to grant annulments on broadened grounds (psychological maturity, lack of sacramental intention) or to admit remarried persons to communion after conscience discernment. Their position is strengthened by synodal authority claims and weakened by doctrinal uncertainty from Rome.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, pastoral_bishops_and_synodal_clergy, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__civic_pastoral_reading, pastoral_bishops_and_synodal_clergy, beneficiary).

% Gain access to sacraments (Eucharist, confession) and pastoral recognition of second marriages without formal annulment, through pastoral discernment and conscience formation. They benefit from the reading's acknowledgment that first marriages may have been defective in ways prior doctrine did not recognize. Their exit is constrained: departure means loss of sacramental participation and communal identity.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, remarried_divorced_catholics, beneficiary,
    organized, biographical, constrained, national).

% Experience loss of normative clarity and institutional authority on marriage doctrine. Their identity is constituted through participation in a Church that teaches indissolubility as metaphysical fact, not pastoral ideal. Doctrinal relativization and inconsistent enforcement (bishops vary; Rome equivocates) undermine the coherence of their self-understanding. Exit means psychological rupture: leaving Catholicism is experienced as self-annihilation, not choice.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, traditional_catholics_identity_locked, payer,
    moderate, biographical, identity_locked, national).

% Cannot obtain consistent pastoral guidance on marriage permanence and sacramental eligibility. They face a Church where doctrine is applied inconsistently: two bishops give opposite annulment verdicts; Rome's position is ambiguous; conscience formation is invoked both to permit and to prohibit remarriage. The constraint's extractive logic is the imposed uncertainty itself: the institution collects pastoral authority without delivering predictable moral framework.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, laity_seeking_doctrinal_clarity, payer,
    powerless, biographical, trapped, national).

% Retains doctrinal supremacy (indissolubility remains official teaching) while operationally accepting regional variation and pastoral expansion. Rome uses ambiguous language ('accompaniment,' 'discernment,' 'internal forum') that appears to permit local adaptation while technically preserving indissolubility doctrine. This dual-position reserves ultimate authority while appearing to embrace mercy.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, roman_curia_and_papal_magisterium, agenda_setter,
    institutional, generational, mobile, universal).

% Hold the sibling reading (marriage as ontological reality constitutively indissoluble, requiring centralized hierarchical adjudication). They are excluded from the civic-pastoral reading's construction of the problem and are treated in this constraint's narrative as doctrinal opponents rather than as co-participants in the same kernel. Their voice is not absent (they publish, preach, petition Rome) but is systematically positioned as 'rigorist' or 'lacking mercy' within the civic-pastoral frame.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, hierarchical_indissolubility_readers, excluded,
    moderate, generational, constrained, national).

% Provide interpretive infrastructure: they author the canonically novel concepts (sacramental intention, psychological maturity, lack of due discretion) that expand annulment grounds without formally amending doctrine. Their role is observational in the constraint's operation but instrumental to its persistence — they provide the intellectual cover for treating old doctrine as newly intelligible.
narrative_ontology:constraint_stakeholder(marriage_sacrament__civic_pastoral_reading, doctrinal_theologians_and_canonists, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_sacrament__civic_pastoral_reading, roman_curia_and_papal_magisterium).
narrative_ontology:fixing_cost_class(marriage_sacrament__civic_pastoral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the crisis of divorced Catholics: acknowledges that first marriages may fail in ways prior doctrine did not recognize; coordinates pastoral mercy with doctrinal continuity by reframing indissolubility as an ideal (aspiration) rather than a constitutive metaphysical fact (necessity); distributes authority for discernment to local clergy rather than centralizing it in Rome.
% TRANSFER_FUNCTION: Transfers authority over marriage adjudication from Roman Curia to local bishops; transfers pastoral permission for remarriage from formal annulment mechanism to expanded grounds for annulment PLUS informal conscience discernment ('internal forum'); transfers doctrinal coherence from traditional laity (who lose stable teaching) to pastoral operators (who gain discretionary interpretive authority).
% ABSENT_VOICES: Hierarchical indissolubility readers (sibling reading) are not absent — they publish and petition — but are systematically positioned as obstacles to mercy rather than as representatives of an alternative coherent reading. The voices NOT in the room are the remarried-after-informal-discernment persons themselves: the constraint speaks about them (as objects of mercy) rather than to them (as agents in their own moral discernment).
% DISAPPEARANCE_RATIONALE: If this pastoral reading collapsed (replaced by strict hierarchical indissolubility), remarried Catholics would lose sacramental access and communal position (painful realignment); traditional Catholics would regain doctrinal certainty and institutional coherence (painful vindication); bishops would lose discretionary authority; the Church would face a coherence gain but a mercy-narrative reversal. The constraint's disappearance would reorganize Catholic self-understanding around indissolubility as metaphysical necessity rather than pastoral aspiration.
% FOUNDING_PROBLEM: First marriages fail due to human limitation, psychological injury, defective intention, or changes in circumstance unforseeable at vows. Prior doctrine treated these failures as moral failures (lack of perseverance, sin) rather than as sacramental defects or capacity constraints. Divorced Catholics were exiled from communion. The problem was the pastoral cruelty and doctrinal implausibility of absolute indissolubility applied to humans who are finite and fallible.
% FOUNDING_PROBLEM_CORROBORATION: Pastoral theologians and canon lawyers from outside Rome's hierarchy attest the founding problem is live and acute (synodal documents, pastoral-practice surveys from dioceses). Rome's own language acknowledges the problem obliquely ('accompaniment,' 'wounded families'). Traditional Catholics and hierarchical indissolubility readers attest the problem is misconceived: the real problem is loss of doctrine and institutional authority, not the perceived cruelty of indissolubility (which they view as metaphysical truth, not cruelty). Divorced Catholics testify to suffering under prior doctrine and relief under pastoral interpretation. The contest is genuine: one party (pastoral-mercy advocates) claims the founding problem is acute; the other party (doctrinal-stability advocates) claims the alleged problem is the symptom of deeper institutional dissolution.
narrative_ontology:disappearance_verdict(marriage_sacrament__civic_pastoral_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__civic_pastoral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__civic_pastoral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_sacrament__civic_pastoral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__civic_pastoral_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.62 at interval end, rising from 0.38) because the constraint's persistence depends on institutional authority accepting doctrinal ambiguity — Rome preserves technical indissolubility teaching while operationally permitting its pastoral exception. The extractive logic is NOT the commission extraction (money moving) but the authority extraction: pastoral discretion is gained at the cost of doctrinal coherence. Suppression is high (0.71) because the constraint requires active institutional suppression of the hierarchical reading — it is not that hierarchical-reading voices are silent, but that within the civic-pastoral frame they are treated as obstacles to mercy rather than as coherent alternative readings. Theater is elevated (0.58) because the constraint maintains formal indissolubility doctrine (theatrical compliance) while operationally permitting its exception through reframed annulment grounds and informal conscience discernment (functional dissolution without formal doctrine change). The measurements show rising extractiveness and theater-ratio over the interval: as pastoral practice expands and regional variation increases, the gap between official doctrine and operative reality widens. The slight downturn in the last projection (both metrics) reflects uncertainty about whether the constraint stabilizes or ruptures under pressure from doctrinal traditionalists demanding coherence.
 *
 * PERSPECTIVAL GAP:
 *   The pastoral-clergy seat and the remarried-Catholic seat should compute as beneficiaries; the traditional-Catholic and clarity-seeking laity seats should compute as targets. The engine's directionality computation from beneficiary/victim + exit yields: pastoral operators (institutional power, mobile exit, beneficiary role) → d near 0.0; remarried Catholics (organized power, constrained exit, beneficiary role) → d low but not near-zero (some exit cost due to identity involvement); traditional Catholics (moderate power, identity-locked exit, payer role) → d near 1.0 (full target); laity-clarity-seekers (powerless, trapped exit, payer role) → d = 1.0 (maximum target). The divergence arises because the constraint coordinates FOR some (remarried Catholics, pastoral operators) and extracts FROM others (identity-locked traditionalists, powerless clarity-seekers) THROUGH THE SAME MECHANISM. The ambiguous doctrine and distributed discernment that liberate remarried persons are the same structures that imprison traditionalists in incoherence and leave ordinary laity adrift.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: pastoral_bishops_and_synodal_clergy (gain discretionary authority, mobile exit, positioned to expand authority further), remarried_divorced_catholics (regain sacraments, some cost to identity but less than loss of communion). Victims: traditional_catholics_identity_locked (lose doctrinal certainty, identity-fused, cannot exit without self-annihilation), laity_seeking_doctrinal_clarity (experience inconsistent enforcement, powerless to resolve it, trapped in ambiguity). The Roman Curia occupies a dual position: as the official doctrinal guardian it appears to benefit (maintains indissolubility teaching); as the operational authority it accepts variable local practice (partially absorbs the payer cost). The constraint's extractiveness is the price imposed on those for whom doctrinal coherence and institutional clarity are identity-constituting. For them, the redistribution of authority from Rome to bishops is not an expansion of pastoral mercy but a fragmentation of the institutional ground on which they stand.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pastoral cruelty of absolute indissolubility applied to finite humans) is contested. Pastoral-mercy advocates attest it is acute and live; doctrinal-stability advocates attest it is misconceived (the real problem is institutional erosion). The constraint's classification as tangled_rope (not pure rope, not pure snare) turns on this: IF the founding problem is genuinely pastoral cruelty, the constraint is a legitimate mercy-coordination, and the extraction from traditionalists is a side effect. IF the founding problem is misconceived (a cover story for institutional power-shift), the constraint is a snare wrapped in mercy-rhetoric, and the apparent coordination is the mechanism through which authority redistributes itself. The omega variables map onto this contest. No single engine computation resolves it; the mandatrophy signal is the simultaneous presence of real coordination (remarried Catholics genuinely re-integrated) and real extraction (traditionalists genuinely disempowered from within a framework that dissolves their coherence). The theater-ratio rise (0.42 to 0.61) indicates that as time passes, the gap between official doctrine and operative exception widens, increasing the performative work required to maintain doctrinal coherence — a classic piton drift. The constraint risks evolving from tangled_rope (coordination + extraction through same mechanism) to piton (performing indissolubility doctrine while operationally dissolving it) if the theater becomes the primary function and the coordination-extraction link breaks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_authenticity,
    'Is the founding problem (pastoral cruelty of absolute indissolubility) a genuine structural tension that permits compassionate resolution, or is it a framing that conceals institutional power-shift away from doctrinal clarity?',
    'Historical and phenomenological analysis: (1) Survey traditional Catholics on whether they experience the constraint as liberation (founding problem solved) or as authority erosion (coherence lost). (2) Analyze whether diocesan variation in pastoral practice tracks genuine regional theological diversity or tracks institutional power migration from Rome to local hierarchy. (3) Examine whether remarried Catholics report genuine integration (social, sacramental, spiritual) or peripheral acceptance with residual exclusion.',
    'If the founding problem is authentic, the constraint is a legitimate tangled_rope: real coordination (mercy + integration) with extractive side effect (loss of coherence for traditionalists). If the founding problem is framing, the constraint is a snare: apparent coordination (mercy rhetoric) cloaks pure institutional power redistribution. Classification diverges sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_authenticity, empirical, 'Whether the constraint''s founding problem reflects genuine pastoral need or conceals institutional power-shift.').

omega_variable(
    doctrine_vs_practice_gap_trajectory,
    'Does the rising theater_ratio (0.42 to 0.61) indicate a temporary mismatch that will resolve via formal doctrine amendment, or a permanent structural feature of the Church''s adaptive governance?',
    'Monitor whether Rome formally amends indissolubility doctrine within the next 15–20 years, or whether doctrinal language remains unchanged while practice diverges indefinitely. If formal amendment occurs, theater recedes and the constraint reclassifies toward snare or rope (doctrine matches operative reality). If divergence persists, theater stabilizes and the constraint becomes piton (performance of indissolubility doctrine while operationally dissolved).',
    'Terminal classification: if doctrine amendments, snare (explicit authority redistribution); if practice divergence persists indefinitely, piton (institutional theater). Tangled_rope is transitional; the constraint cannot indefinitely hold coordination + extraction through a mechanism (doctrinal ambiguity) that cannot indefinitely persist without institutional degradation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_practice_gap_trajectory, empirical, 'Whether doctrinal-practice divergence will formally resolve or permanently persist.').

omega_variable(
    identity_lock_mechanism_church_departure,
    'Is the measured identity_lock on traditional Catholics (exit = self-annihilation) experienced as religiously constitutive, or would departure from Catholicism permit psychological continuity under alternative Christian frameworks?',
    'Post-exit analysis: survey traditional Catholics who have left over doctrinal disagreement on marriage; measure whether they report identity rupture or identity continuity (adoption of Eastern Orthodox, traditionalist Catholic, or alternative Protestant identities that preserve doctrinal coherence). If continuity is available, identity_lock is not absolute but relative — they remain locked to doctrinal coherence, not locked to Catholicism per se.',
    'If identity rupture is reported, the measured identity-lock is structurally real and suppression is genuine; if identity continuity is reported, the suppression mechanism is institutional rather than existential — the constraint extracts coherence from traditional Catholics while they remain capable of finding it elsewhere. This would reclassify the suppression vector.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_church_departure, empirical, 'Whether traditional-Catholic identity-lock is absolute (Catholicism-specific) or relative (doctrinal-coherence-seeking, portable to other frameworks).').

omega_variable(
    hierarchical_reading_as_coercive_shadow,
    'Does the hierarchical-indissolubility reading remain a live, coherent alternative position, or has it been transformed (through institutional marginalizing and rhetorical positioning as ''rigorist'') into a coercive shadow that can no longer be authentically held without social penalty?',
    'Institutional analysis: measure the career and reputational cost to clergy, theologians, and lay advocates of publicly holding the hierarchical reading. If costs are negligible (positions available, publishing permitted, hierarchical readers respected as serious interlocutors), the reading remains live. If costs are substantial (marginalization, career damage, rhetorical dismissal as lacking mercy), the reading is being suppressed through social coercion while remaining formally permitted.',
    'If live, the readings coexist; the constraint is community conflict with two coherent poles. If suppressed, the constraint is extractive: the hierarchical reading is being forcibly transformed into a subordinate position, and suppression of that reading becomes a structural feature of the constraint''s maintenance. This would strengthen the snare classification and identify suppression as targeting cognitive/doctrinal positions, not merely behavioral compliance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hierarchical_reading_as_coercive_shadow, empirical, 'Whether hierarchical-indissolubility reading remains live or is being socially suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__civic_pastoral_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_sacrament__civic_pastoral_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement_basis(marr_tr_t0, observed).
narrative_ontology:measurement(marr_tr_t5, marriage_sacrament__civic_pastoral_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement_basis(marr_tr_t5, observed).
narrative_ontology:measurement(marr_tr_t10, marriage_sacrament__civic_pastoral_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(marr_tr_t10, observed).
narrative_ontology:measurement(marr_tr_t20, marriage_sacrament__civic_pastoral_reading, theater_ratio, 20, 0.59).
narrative_ontology:measurement_basis(marr_tr_t20, observed).
narrative_ontology:measurement(marr_tr_t30, marriage_sacrament__civic_pastoral_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement_basis(marr_tr_t30, observed).
narrative_ontology:measurement(marr_tr_t40, marriage_sacrament__civic_pastoral_reading, theater_ratio, 40, 0.58).
narrative_ontology:measurement_basis(marr_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(marr_be_t0, observed).
narrative_ontology:measurement(marr_be_t5, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(marr_be_t5, observed).
narrative_ontology:measurement(marr_be_t10, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(marr_be_t10, observed).
narrative_ontology:measurement(marr_be_t20, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(marr_be_t20, observed).
narrative_ontology:measurement(marr_be_t30, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement_basis(marr_be_t30, observed).
narrative_ontology:measurement(marr_be_t40, marriage_sacrament__civic_pastoral_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(marr_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(marr_su_t0, observed).
narrative_ontology:measurement(marr_su_t5, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 5, 0.61).
narrative_ontology:measurement_basis(marr_su_t5, observed).
narrative_ontology:measurement(marr_su_t10, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(marr_su_t10, observed).
narrative_ontology:measurement(marr_su_t20, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(marr_su_t20, observed).
narrative_ontology:measurement(marr_su_t30, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement_basis(marr_su_t30, observed).
narrative_ontology:measurement(marr_su_t40, marriage_sacrament__civic_pastoral_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(marr_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__civic_pastoral_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__civic_pastoral_reading, 0.12).
narrative_ontology:affects_constraint(marriage_sacrament__civic_pastoral_reading, marriage_sacrament__hierarchical_indissolubility_reading).

% DUAL FORMULATION NOTE:
% The marriage-sacrament kernel admits two structurally distinct readings: this one (civic-pastoral) and its sibling (hierarchical-indissolubility). They are NOT observer-relative interpretations of a neutral fact. They are incompatible commitments about what marriage IS (relationship subject to failure vs. ontological indissoluble fact) and who adjudicates it (distributed pastoral discernment vs. centralized hierarchical judgment). Each reading instantiates a different constraint with different ε, different beneficiaries/victims, different mechanisms. They coexist as live positions held by different factions within Catholicism, neither foreclosing the other within the institution as a whole, but each one foreclosing the other within any single person's framework. The sibling reading's ε is lower (higher doctrinal stability, less ambiguity, clearer enforcement) but also its resistance is lower (fewer beneficiaries, greater institutional coherence cost). This constraint influences the sibling by creating pastoral and institutional pressure that makes strict hierarchical indissolubility politically difficult to enforce, even as official doctrine technically affirms it. The affects_constraints link routes the contamination analysis: doctrinal clarity erosion in this constraint degrades the authority structure that the sibling constraint depends on.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
