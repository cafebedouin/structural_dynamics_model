% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope — Progressive Textualist Reading (Amendment-Gated Expansion)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint models the progressive-textualist reading of the equality
 *   clause's contested scope: the text contains a genuine equality principle,
 *   and that principle's application scope can legitimately expand — but only
 *   through the democratic amendment process (Article V supermajorities), not
 *   through judicial reinterpretation of existing text. This is a
 *   bounded-universalist middle position between the restrictive-originalist
 *   reading (equality applies only to the 18th-century propertied-white-male
 *   political class) and the expansive-universalist reading (equality is
 *   self-evident and applies to all humans regardless of when courts get
 *   around to recognizing it). Each of these three readings is a separate
 *   constraint with its own ε, its own beneficiary/victim structure, and its
 *   own classification; this file models only the progressive-textualist
 *   reading, per the ε-invariance decomposition rule.
 *
 * KEY AGENTS:
 *   - enfranchised_amendment_era_groups: beneficiary — durable, court-proof recognition secured via ratified amendment
 *   - groups_awaiting_amendment_recognition: primary payer — trapped without judicial recourse, must build supermajority coalition
 *   - judicially_recognizable_but_unamended_classes: secondary payer — plausible claim under text, foreclosed from interpretive remedy
 *   - legislative_coalition_builders: agenda_setter/beneficiary — controls pace and content of scope expansion, gains legitimacy as the designated vehicle
 *   - federal_judiciary: agenda_setter/observer — self-restrains from expanding scope by doctrine, defers to amendment channel
 *   - constitutional_stability_beneficiaries: beneficiary — gains predictability and reduced doctrinal volatility
 *   - constitutional_law_scholars: observer — evaluates whether the gate is principled or captured
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.42).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.38).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope — Progressive Textualist Reading (Amendment-Gated Expansion)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '06dbd954-3cac-42b3-8a7a-2856543bf4e5').
narrative_ontology:cs_kernel_codification('06dbd954-3cac-42b3-8a7a-2856543bf4e5', fixed_text).
narrative_ontology:cs_authority_grounding('06dbd954-3cac-42b3-8a7a-2856543bf4e5', lineage).
narrative_ontology:cs_interpretation_layer_present('06dbd954-3cac-42b3-8a7a-2856543bf4e5').
narrative_ontology:cs_reading_relation('06dbd954-3cac-42b3-8a7a-2856543bf4e5', equality_clause_scope__restrictive_originalist, coexists_with).
narrative_ontology:cs_reading_relation('06dbd954-3cac-42b3-8a7a-2856543bf4e5', equality_clause_scope__expansive_universalist, influences).
narrative_ontology:cs_axiom('06dbd954-3cac-42b3-8a7a-2856543bf4e5', foundational, scope_expansion_requires_supermajority_democratic_consent).
narrative_ontology:cs_axiom_status(scope_expansion_requires_supermajority_democratic_consent, holdable).
narrative_ontology:cs_axiom_grounding('06dbd954-3cac-42b3-8a7a-2856543bf4e5', scope_expansion_requires_supermajority_democratic_consent, conventional).
narrative_ontology:cs_axiom('06dbd954-3cac-42b3-8a7a-2856543bf4e5', foundational, text_contains_general_principle_not_frozen_at_ratification).
narrative_ontology:cs_axiom_status(text_contains_general_principle_not_frozen_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('06dbd954-3cac-42b3-8a7a-2856543bf4e5', text_contains_general_principle_not_frozen_at_ratification, conventional).
narrative_ontology:cs_axiom('06dbd954-3cac-42b3-8a7a-2856543bf4e5', secondary, judicial_scope_expansion_is_illegitimate_absent_amendment).
narrative_ontology:cs_axiom_status(judicial_scope_expansion_is_illegitimate_absent_amendment, holdable).
narrative_ontology:cs_axiom_grounding('06dbd954-3cac-42b3-8a7a-2856543bf4e5', judicial_scope_expansion_is_illegitimate_absent_amendment, instrumental).
narrative_ontology:cs_reference_frame('06dbd954-3cac-42b3-8a7a-2856543bf4e5', textual_principle_with_amendment_gated_expansion).
narrative_ontology:cs_drift_state('06dbd954-3cac-42b3-8a7a-2856543bf4e5', post_civil_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('06dbd954-3cac-42b3-8a7a-2856543bf4e5', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, enfranchised_amendment_era_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, constitutional_stability_beneficiaries).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, legislative_coalition_builders).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, groups_awaiting_amendment_recognition).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, judicially_recognizable_but_unamended_classes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose inclusion under the equality principle was secured through a ratified amendment (e.g. the Reconstruction Amendments, the Nineteenth Amendment). Their standing is textually locked in and cannot be revoked by a shift in judicial philosophy; they benefit from the durability the amendment process provides, at the cost of having had to mobilize supermajority coalitions to get there.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, enfranchised_amendment_era_groups, beneficiary,
    organized, generational, constrained, national).

% Groups whose claim to equal treatment is textually plausible but not yet the subject of a ratified amendment. Under this reading, courts are structurally barred from recognizing their claims as already covered by the existing text; they must build a supermajority coalition across two-thirds of Congress and three-fourths of the states before relief arrives. In the interim they bear the full cost of exclusion with no judicial remedy available.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, groups_awaiting_amendment_recognition, payer,
    powerless, biographical, trapped, national).

% Classes whose exclusion an expansive-universalist court would likely correct by interpretation, but which this reading requires be corrected only by amendment. They experience the gap between what they believe the equality principle already logically requires and what the enforcement mechanism will actually deliver, absent a supermajority vote.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judicially_recognizable_but_unamended_classes, payer,
    moderate, biographical, constrained, national).

% Elected officials and organized political movements who administer the amendment process — drafting proposed amendments, assembling coalitions, running ratification campaigns. They control the pace and content of scope expansion and derive political capital and legitimacy from being the vehicle through which equality claims are recognized, rather than ceding that role to courts.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, legislative_coalition_builders, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, legislative_coalition_builders, beneficiary).

% Institutions and actors (courts, bar associations, long-horizon political economy participants) that benefit from predictable, textually-anchored constitutional meaning that does not swing with judicial composition. They gain legitimacy and reduced volatility from binding scope changes to the harder amendment channel.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_stability_beneficiaries, beneficiary,
    institutional, civilizational, analytical, national).

% Courts under this reading are bound to apply the equality principle's textual scope as fixed at ratification, absent later amendment expanding it; they decline to treat unamended text as reaching newly claimed classes even where the underlying principle seems to apply. Their institutional posture is one of self-restraint, deferring scope questions to the amendment process rather than resolving them through doctrine.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, federal_judiciary, observer).

% Academics and commentators who evaluate whether the amendment-gated reading is a principled middle path or a mechanism that systematically defers costs onto groups too small or too dispersed to assemble supermajority coalitions.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__progressive_textualist, legislative_coalition_builders).
narrative_ontology:fixing_cost_class(equality_clause_scope__progressive_textualist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable, and democratically legitimated channel for expanding who counts within the equality principle — preventing scope from being unilaterally redefined by shifting judicial majorities and requiring durable cross-factional consent before the constitutional promise is textually extended.
% TRANSFER_FUNCTION: Moves the burden of proof and mobilization cost onto groups seeking recognition: they must build supermajority coalitions (two-thirds of Congress, three-fourths of the states) rather than winning recognition through litigation, while groups already covered by ratified amendments enjoy durable, court-proof protection.
% ABSENT_VOICES: Groups whose equality claims are textually plausible under an expansive reading but who lack the political organization to mount a national amendment campaign are structurally unable to secure relief through this reading's designated channel; they are heard in litigation (where an expansive-universalist court might rule for them) but that channel is foreclosed here.
% DISAPPEARANCE_RATIONALE: If this reading's amendment-gate were abandoned overnight, some parties (courts, unamended-but-arguably-covered classes) would treat this as liberating — the equality principle would apply immediately to newly recognized classes via interpretation. Other parties (coalition-builders whose institutional role depends on being the vehicle of change, and those who value textual predictability) would treat this as a rupture undermining the legitimacy basis of the entire constitutional order. The two camps genuinely disagree about whether the world 'rearranges' or 'corrects.'
% FOUNDING_PROBLEM: The Founders and subsequent ratifying generations wrote a general equality principle into the text while historically limiting its practical application to a narrow class of persons; the amendment process was designed to be the constitutionally sanctioned mechanism for later generations to expand that class without either freezing the text forever or letting scope be redefined by unelected judges reading their own values into ambiguous text.
% FOUNDING_PROBLEM_CORROBORATION: Legislative historians and amendment-process scholars attest the mechanism functioned as designed for the Reconstruction and Nineteenth Amendments, delivering durable recognition through supermajority consent. Civil rights historians and excluded-group advocates, from outside the coalition-builder seat, attest the mechanism has also functioned to delay recognition for decades past the point where the underlying principle was broadly understood to require it (e.g. the stalled Equal Rights Amendment), suggesting the founding problem of 'legitimate but bounded expansion' has partially calcified into a veto mechanism for entrenched minorities of states.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, contested).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than low or severe: the reading genuinely coordinates a legitimate problem (preventing scope from being redefined by transient judicial majorities) but also imposes a real, uneven cost on classes whose claims are textually plausible yet politically unable to assemble Article V supermajorities — a structurally high bar (two-thirds of Congress, three-fourths of the states) that functions differently depending on a group's political organization, not the merit of its claim. Suppression is moderate (0.38): the mechanism does not coercively suppress alternative readings so much as it forecloses one specific remedy channel (judicial recognition) while leaving the political channel open, which is real but softer suppression than an outright ban on advocacy. Theater ratio is low-moderate (0.22): most of the mechanism's operation is genuine coordination and genuine political mobilization, though some 'this is how equality legitimately expands' rhetoric performs constitutional continuity that masks how long unamended-but-plausible claims can sit unresolved (e.g. the decades-long stall of the Equal Rights Amendment).
 *
 * PERSPECTIVAL GAP:
 *   From the coalition-builder and stable-institution seats, this reading looks like a Rope: a genuine, legitimate, non-coercive mechanism for expanding constitutional meaning that avoids both ossification (originalism) and judicial overreach (universalism). From the seat of a group whose plausible equality claim sits unresolved for decades because it cannot assemble a supermajority, the same structure looks like a Tangled Rope shading toward Snare: real coordination function, but the cost of that function is disproportionately and durably borne by politically weaker claimants, and the mechanism requires active enforcement (courts declining to reach the merits) to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   Coalition-builders and courts sit as agenda-setters: they administer and enforce the amendment-gate, and courts additionally benefit from a reduced doctrinal burden (they are not asked to resolve scope questions through interpretation). Enfranchised amendment-era groups and constitutional-stability beneficiaries sit at the low-d end: durable protection, reduced volatility. Groups awaiting amendment recognition sit at the high-d end: trapped, bearing the ongoing cost of exclusion with no judicial remedy, dependent entirely on their capacity to build a national supermajority coalition — a capacity that correlates with size, geographic distribution, and existing political power, not with the strength of their equality claim. Judicially-recognizable-but-unamended classes sit closer to the middle: their claim would likely succeed under a different reading, so the gap between what they are owed under the text (as they read it) and what this reading actually delivers is itself the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a legitimate, non-judicial channel for expanding who the equality principle covers — remains partially live (the amendment mechanism has successfully delivered recognition, e.g. the Reconstruction and Nineteenth Amendments) and partially calcified (the same supermajority threshold that legitimates successful expansions also indefinitely stalls claims that lack broad-based political momentum, regardless of textual or moral merit). Classifying this as Tangled Rope rather than pure Rope or pure Snare prevents two mislabeling errors: treating the amendment-gate as pure extraction would erase its genuine legitimating and stabilizing function; treating it as pure coordination would erase the real, asymmetric cost imposed on groups whose claims sit in permanent limbo because they cannot muster Article V supermajorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_threshold_as_legitimate_gate_or_captured_veto,
    'Is the Article V supermajority threshold a legitimate legitimacy-conferring mechanism, or has it become a captured veto point that indefinitely stalls textually plausible equality claims lacking broad political momentum (e.g. the multi-decade stall of the Equal Rights Amendment)?',
    'Comparative historical analysis of amendment attempts: which succeeded, which stalled, and whether stall correlates with the merit/textual-plausibility of the claim versus the claimant group''s political organization and geographic distribution across states.',
    'If the threshold functions as a genuine legitimacy filter, this reading is closer to a Rope with moderate extraction as an acceptable coordination cost. If it functions as a captured veto correlating with political weakness rather than claim merit, the classification shifts toward Snare for the stalled-claimant seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_threshold_as_legitimate_gate_or_captured_veto, conceptual, 'Whether the supermajority amendment threshold is a legitimate gate or a captured veto mechanism.').

omega_variable(
    kernel_reading_selection_basis,
    'This story instantiates the progressive_textualist reading among three contested readings of the same equality-clause kernel (restrictive_originalist, progressive_textualist, expansive_universalist). What determines which reading a given court, legislature, or historical actor actually adopts, and is that selection itself principled or outcome-driven?',
    'Analysis of judicial and legislative opinions across the historical interval to determine whether reading-selection correlates with independent jurisprudential commitments or with the substantive outcome the selector prefers in the specific case at hand.',
    'If reading-selection is outcome-driven, the progressive_textualist reading''s ε and classification may themselves be unstable across cases — used to block relief in some contexts (originalist-adjacent) and to justify relief in others (universalist-adjacent), which would suggest the ''reading'' is doing less independent work than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_basis, conceptual, 'Whether selection among the three kernel readings is principled or outcome-driven — the committer-axis disagreement is located here.').

omega_variable(
    textual_scope_ambiguity_at_ratification,
    'Did the ratifying generations of the relevant equality-clause text (e.g. the Fourteenth Amendment''s Equal Protection Clause) intend a principle capable of textually-anchored future expansion via amendment, or did they intend the clause''s application to be fixed at ratification, with any expansion requiring an entirely new textual provision rather than ''growth'' of the existing one?',
    'Historical linguistic and drafting-history analysis of ratification-era debates, comparing rhetoric of principle-application (''all persons'') against contemporaneous exclusionary practice.',
    'If the ratifying generation intended a genuinely general principle whose scope was expected to be worked out over time via the amendment mechanism, this reading is well-grounded. If the text was understood as bounded at ratification with no anticipated expansion mechanism, this reading over-claims textual warrant for calling amendment-based expansion ''application scope growth'' rather than new-provision creation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_scope_ambiguity_at_ratification, empirical, 'Whether the amendment process was originally understood as expanding the existing equality principle''s scope or creating new textual provisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1868, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.1).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.13).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__progressive_textualist, theater_ratio, 1965, 0.16).
narrative_ontology:measurement(equa_tr_t1982, equality_clause_scope__progressive_textualist, theater_ratio, 1982, 0.25).
narrative_ontology:measurement(equa_tr_t2000, equality_clause_scope__progressive_textualist, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(equa_tr_t2020, equality_clause_scope__progressive_textualist, theater_ratio, 2020, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.3).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.33).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__progressive_textualist, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(equa_be_t1982, equality_clause_scope__progressive_textualist, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement(equa_be_t2000, equality_clause_scope__progressive_textualist, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(equa_be_t2020, equality_clause_scope__progressive_textualist, base_extractiveness, 2020, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.25).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.28).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__progressive_textualist, suppression_requirement, 1965, 0.32).
narrative_ontology:measurement(equa_su_t1982, equality_clause_scope__progressive_textualist, suppression_requirement, 1982, 0.4).
narrative_ontology:measurement(equa_su_t2000, equality_clause_scope__progressive_textualist, suppression_requirement, 2000, 0.36).
narrative_ontology:measurement(equa_su_t2020, equality_clause_scope__progressive_textualist, suppression_requirement, 2020, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equality_clause_scope kernel, each authored as a separate story per the ε-invariance decomposition rule. restrictive_originalist claims scope is fixed at ratification to the 18th-century propertied-white-male class (low ε if internally consistent, but high suppression of excluded claims). expansive_universalist claims scope already covers all humans regardless of historical exclusion and treats judicial non-recognition as illegitimate withholding (likely higher ε, snare-leaning, from the excluded group's perspective, since the universalist reading treats current textual application as actively wrongful). This progressive_textualist reading occupies the bounded middle: moderate ε (0.42), tangled_rope, because it grants a real expansion mechanism while gating it behind a high political cost that falls unevenly on politically weak claimants. All three files should be read together; none of the three ε values are commensurable measurements of 'the equality clause' as a single object — they are measurements of three structurally distinct claims about what the clause is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
