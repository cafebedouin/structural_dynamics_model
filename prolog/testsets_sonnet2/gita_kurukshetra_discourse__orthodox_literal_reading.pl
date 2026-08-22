% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading of the Bhagavad Gita's Kurukshetra Discourse — Caste-Duty and Righteous War Mandate
 *   domain: religious/social/ethical
 *
 * SUMMARY:
 *   This story authors the orthodox literal reading of Krishna's discourse to
 *   Arjuna at Kurukshetra: the text is read as a direct theological mandate
 *   that caste-assigned duty (varnashrama dharma) is divinely ordained, that
 *   a kshatriya's duty to fight — even against kin — overrides personal moral
 *   revulsion, and that Brahmins hold the legitimate authority to interpret
 *   what any caste's duty requires. Under this reading, the coordination
 *   function (resolving conscience-crisis in socially mandated roles) is
 *   bundled with an extraction function (fixing lower castes' labor role and
 *   insulating warrior violence from ethical review), administered and
 *   enforced by an interpretive elite who also benefit from the arrangement.
 *   This is a KERNEL READING: the same text, read allegorically (internal
 *   spiritual struggle, no literal caste mandate) or through a universalist
 *   bhakti lens (devotion path-independent of caste), yields structurally
 *   different constraints with different — in the allegorical case, near-zero
 *   — extraction. Those are separate stories (gandhian_allegorical_reading,
 *   universalist_devotional_reading), not alternate measurements of this one;
 *   ε here is authored strictly for the standing orthodox-literal arrangement
 *   as its own adherents and institutions maintain it.
 *
 * KEY AGENTS:
 *   - brahmin_interpretive_class: agenda_setter/beneficiary (institutional/arbitrage) — controls canonical interpretation, collects deference and patronage
 *   - kshatriya_ruling_warriors: beneficiary (powerful/constrained) — violence insulated from moral scrutiny via caste-duty framing
 *   - orthodox_temple_authorities: beneficiary/agenda_setter (institutional/arbitrage) — administers caste-organized ritual life
 *   - shudra_and_lower_caste_laborers: payer (powerless/trapped) — confined to servile duty by scriptural warrant
 *   - war_dead_and_conscripted_combatants: payer (powerless/trapped) — bear lethal cost of righteous-war doctrine
 *   - caste_mobility_seekers: payer (powerless/trapped) — mobility discouraged as spiritually hazardous
 *   - reformist_and_bhakti_movements: excluded (organized/constrained) — alternative readings denied institutional standing
 *   - comparative_religious_scholars: observer (analytical/analytical) — trace textual strata and political use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.71).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading of the Bhagavad Gita's Kurukshetra Discourse — Caste-Duty and Righteous War Mandate").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/social/ethical").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '526be2fd-1439-4a5c-8835-084c0819bdec').
narrative_ontology:cs_kernel_codification('526be2fd-1439-4a5c-8835-084c0819bdec', fixed_text).
narrative_ontology:cs_authority_grounding('526be2fd-1439-4a5c-8835-084c0819bdec', lineage).
narrative_ontology:cs_interpretation_layer_present('526be2fd-1439-4a5c-8835-084c0819bdec').
narrative_ontology:cs_reading_relation('526be2fd-1439-4a5c-8835-084c0819bdec', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('526be2fd-1439-4a5c-8835-084c0819bdec', gita_kurukshetra_discourse__universalist_devotional_reading, influences).
narrative_ontology:cs_axiom('526be2fd-1439-4a5c-8835-084c0819bdec', foundational, varna_duty_is_birth_fixed_and_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_duty_is_birth_fixed_and_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('526be2fd-1439-4a5c-8835-084c0819bdec', varna_duty_is_birth_fixed_and_divinely_ordained, theological).
narrative_ontology:cs_axiom('526be2fd-1439-4a5c-8835-084c0819bdec', foundational, kshatriya_violence_in_dharmic_war_is_morally_obligatory_not_merely_permitted).
narrative_ontology:cs_axiom_status(kshatriya_violence_in_dharmic_war_is_morally_obligatory_not_merely_permitted, holdable).
narrative_ontology:cs_axiom_grounding('526be2fd-1439-4a5c-8835-084c0819bdec', kshatriya_violence_in_dharmic_war_is_morally_obligatory_not_merely_permitted, deontological).
narrative_ontology:cs_axiom('526be2fd-1439-4a5c-8835-084c0819bdec', secondary, brahmin_lineage_holds_exclusive_interpretive_authority_over_dharma_texts).
narrative_ontology:cs_axiom_status(brahmin_lineage_holds_exclusive_interpretive_authority_over_dharma_texts, holdable).
narrative_ontology:cs_axiom_grounding('526be2fd-1439-4a5c-8835-084c0819bdec', brahmin_lineage_holds_exclusive_interpretive_authority_over_dharma_texts, conventional).
narrative_ontology:cs_reference_frame('526be2fd-1439-4a5c-8835-084c0819bdec', vedic_varnashrama_social_order).
narrative_ontology:cs_drift_state('526be2fd-1439-4a5c-8835-084c0819bdec', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('526be2fd-1439-4a5c-8835-084c0819bdec', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_ruling_warriors).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, orthodox_temple_authorities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_and_lower_caste_laborers).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_and_conscripted_combatants).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, caste_mobility_seekers).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varnashrama_dharma_is_divinely_ordained).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, svadharma_supersedes_individual_moral_qualm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls Sanskrit textual transmission, commentary tradition (bhashya), and ritual authority to pronounce on dharma. Administers which readings of Krishna's discourse to Arjuna count as authoritative, and derives social standing and material support (patronage, temple offerings, teaching fees) from being the sole legitimate interpreters of a text that assigns them the highest station in the very order it sanctifies.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class, beneficiary).

% Warrior-ruler class whose violence in war and governance is framed as sacred duty (svadharma) rather than moral choice, insulating military and political power from ethical scrutiny. Arjuna's resolved doubt on the battlefield stands as the text's central exemplar: killing kin is not merely permitted but obligatory when it is one's caste-duty, which retroactively legitimates kshatriya violence across history.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_ruling_warriors, beneficiary,
    powerful, generational, constrained, national).

% Institutions that administer ritual life organized around caste-appropriate duties; the literal reading supplies scriptural warrant for continuing to allocate ritual roles, temple access, and social status by birth-caste, reinforcing institutional structures that these authorities administer and from which they draw revenue and standing.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, orthodox_temple_authorities, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__orthodox_literal_reading, orthodox_temple_authorities, agenda_setter).

% Assigned to servile duty (shudra dharma) by birth under this reading, with the text's authority invoked to frame confinement to menial labor and exclusion from higher ritual/social roles as spiritually correct rather than as an imposed social order. Exit requires either textual reinterpretation they are denied authority to perform, or exit from the tradition entirely, at high social cost.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, shudra_and_lower_caste_laborers, payer,
    powerless, generational, trapped, national).

% Foot soldiers and conscripted fighters on both sides of dharmic wars whose deaths are retroactively sanctified as righteous outcomes of caste-duty rather than counted as costs to be minimized; they bear the lethal consequence of a doctrine that removes moral hesitation from those who order or perform violence.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, war_dead_and_conscripted_combatants, payer,
    powerless, immediate, trapped, regional).

% Individuals seeking to change occupation, marry across caste lines, or otherwise depart from birth-assigned duty. The literal reading's insistence that one's own imperfectly-performed dharma is better than another's well-performed dharma is invoked against them to discourage social mobility as spiritually hazardous.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, caste_mobility_seekers, payer,
    powerless, biographical, trapped, national).

% Devotional and reform movements that read the same verses as teaching caste-independent surrender to the divine would contest the literal caste-duty reading directly, but under orthodox institutional control their alternative readings are treated as heterodox innovations rather than as equally legitimate interpretations of the same text.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, reformist_and_bhakti_movements, excluded,
    organized, generational, constrained, national).

% Study the historical layering of the text, the plausible dating of caste-duty verses relative to other strata, and the political uses to which the literal reading has been put across centuries, without a stake in which reading prevails ritually.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, comparative_religious_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpretive_class).
narrative_ontology:fixing_cost_class(gita_kurukshetra_discourse__orthodox_literal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, textually-anchored resolution to the problem of moral paralysis in socially mandated roles: it tells a warrior facing conscience-crisis that fulfilling caste-assigned duty is itself the spiritually correct act, and it gives a stratified society a single canonical justification for maintaining role-differentiation across generations.
% TRANSFER_FUNCTION: Moves interpretive authority and the social deference that follows it to the Brahmin commentarial class; moves moral legitimacy and freedom from role-constraint to kshatriya rulers and warriors; moves confinement to low-status labor, exclusion from higher ritual participation, and blocked mobility onto lower-caste groups; moves lives, in the immediate sense, from combatants to the battlefield in wars the doctrine frames as righteous rather than as tragedies to be prevented.
% ABSENT_VOICES: Bhakti and reform movements that read the same dialogue as teaching devotion accessible regardless of birth are not given interpretive standing within the orthodox institutional structure; lower-caste communities whose duty is defined for them by this reading have historically had no voice in the commentarial tradition that fixes what their dharma is.
% DISAPPEARANCE_RATIONALE: If the orthodox literal reading lost its institutional and social force, caste-based allocation of ritual and occupational roles would lose its primary scriptural warrant, kshatriya violence would lose a specific theological insulation from moral scrutiny, and Brahmin interpretive monopoly over this text would lose its exclusive claim — social mobility movements and alternative (bhakti, allegorical) readings would gain ground that the literal reading currently forecloses.
% FOUNDING_PROBLEM: The dialogue was composed to resolve a genuine crisis: how should a person act rightly when duty (to kin, to caste-role, to king) and personal moral revulsion conflict, particularly at the threshold of a devastating war. The text offers a resolution: act according to one's own dharma without attachment to outcome.
% FOUNDING_PROBLEM_CORROBORATION: Brahmin commentators and kshatriya-descended institutions attest the caste-duty framework remains the correct and still-live resolution. Comparative religious historians and reformist movements from outside the beneficiary lineage — including bhakti-tradition teachers historically excluded from commentarial authority and modern scholars tracing textual strata — attest that the caste-duty verses reflect a specific social order of composition rather than an eternal metaphysical necessity, and that the crisis-of-conscience problem the dialogue addresses can be and has been resolved without caste-based duty allocation.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.71) reflects the doctrine's dual cost: material/status confinement of lower castes and moral insulation of violence for the warrior class, both channeled through and legitimated by a single interpretive authority. Suppression (0.78) is high because the reading's persistence depends on excluding competing interpretations (allegorical, bhakti) from institutional legitimacy — not merely on textual ambiguity but on active gatekeeping of who may authoritatively interpret. Theater ratio rises over the interval (0.20 to 0.42) as literal caste enforcement in many regions became more performative/ritual than materially load-bearing over the last centuries, even as the underlying doctrine's legitimating function for social hierarchy and its rhetorical use to sanction violence persisted. Accessibility collapse (0.62) reflects that once the orthodox literal reading is institutionally established as canonical, alternative readings require breaking from an entrenched commentarial tradition, a nontrivial but not impossible act (contrast a genuine mountain's near-total collapse). Resistance (0.58) captures centuries of reform, bhakti, and anti-caste movements actively contesting the doctrine from within and outside the tradition.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin interpretive class and orthodox temple authorities sit near the full-beneficiary end: they administer the reading, face no exit cost, and collect status/material benefit from the arrangement's persistence. Kshatriya rulers benefit specifically through moral insulation of violence — a narrower but real benefit. Lower-caste laborers, caste mobility seekers, and war dead/combatants sit near the full-target end: trapped exit options (leaving the caste system or the war both carry severe social/physical cost), and the doctrine's operation directly constrains or endangers them. Reformist movements are excluded rather than positioned along the beneficiary-victim axis — their objection is structurally locked out of the interpretive process itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (resolving conscience-crisis between duty and moral revulsion) is genuinely old and, in the abstract, potentially still live for individuals facing role-conflict. But the specific mechanism this reading retains — caste as the fixed content of duty — is contested as obsolete by parties outside the beneficiary lineage: bhakti teachers and modern scholars argue the underlying ethical problem can be resolved without birth-based role assignment. The tangled_rope classification (rather than pure snare) is warranted because a genuine coordination problem (how to act when duty and conscience conflict) is real and is being solved for kshatriya_ruling_warriors and, notionally, for society's role-differentiation needs — but the same mechanism extracts asymmetrically from those with no say in defining their own duty. Classifying this as pure snare would erase the doctrine's genuine psychological/social coordination function for those it does not victimize; classifying it as pure rope would erase the caste confinement and interpretive monopoly that the beneficiary/victim declarations establish.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_allegorical_textual_intent,
    'Does the dialogue''s original composition and reception context support a literal caste-duty/literal-war reading, or was this reading a later theological/political overlay serving specific institutional interests?',
    'Historical-critical textual dating and strata analysis, comparison with surrounding Mahabharata narrative context, and analysis of commentarial history (Shankara, Ramanuja, and later reformist commentators) to trace when the literal caste-duty reading became institutionally dominant relative to competing readings.',
    'If the literal reading is shown to be a later institutional overlay rather than original intent, its claim to represent the text''s authoritative meaning weakens substantially, strengthening the case that this reading survives primarily through interpretive monopoly rather than textual necessity — supporting reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_vs_allegorical_textual_intent, conceptual, 'Whether the literal caste-mandate reading reflects original textual intent or later institutional construction.').

omega_variable(
    committer_kernel_reading_selection,
    'This story is one of three readings (orthodox_literal, gandhian_allegorical, universalist_devotional) of the same Kurukshetra dialogue. Is the orthodox literal reading''s continued institutional dominance a function of the text''s own content, or of which reading historically captured Brahminical and royal institutional power first?',
    'Comparative study of which reading held institutional dominance in which historical period and region, cross-referenced with which reading served the interpretive and material interests of the institutions promoting it.',
    'If institutional capture (rather than textual clarity) explains the literal reading''s historical dominance, the reading''s persistence is better explained by the interpretive monopoly of its beneficiaries than by the text compelling this reading over its siblings — reinforcing the tangled_rope classification''s extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_reading_selection, conceptual, 'Committer-frame note: why this reading achieved institutional dominance among the three sibling readings of the same kernel.').

omega_variable(
    svadharma_as_liberation_or_confinement,
    'Is svadharma (one''s own duty) intended, even within the literal reading, as a liberating framework (act without attachment to outcome, within any role) or as a confining one (the role itself is fixed by birth and non-negotiable)?',
    'Close reading of the specific verses on svadharma versus paradharma (chapter 3, verse 35 and parallels) in light of the broader philosophical apparatus (nishkama karma, the doctrine of non-attachment) that the text develops elsewhere.',
    'If svadharma is read as compatible with chosen vocation rather than birth-fixed caste, the caste-confinement component of this reading weakens considerably, moving the classification toward rope; if birth-fixation is textually load-bearing, the tangled_rope/extraction reading is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(svadharma_as_liberation_or_confinement, conceptual, 'Whether svadharma within the literal reading itself requires birth-fixed caste role or merely non-attached action within a chosen role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(gita_tr_t0, projected).
narrative_ontology:measurement(gita_tr_t400, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(gita_tr_t400, projected).
narrative_ontology:measurement(gita_tr_t800, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 800, 0.3).
narrative_ontology:measurement_basis(gita_tr_t800, projected).
narrative_ontology:measurement(gita_tr_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1200, 0.35).
narrative_ontology:measurement_basis(gita_tr_t1200, projected).
narrative_ontology:measurement(gita_tr_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1600, 0.39).
narrative_ontology:measurement_basis(gita_tr_t1600, observed).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement_basis(gita_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(gita_be_t0, projected).
narrative_ontology:measurement(gita_be_t400, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 400, 0.6).
narrative_ontology:measurement_basis(gita_be_t400, projected).
narrative_ontology:measurement(gita_be_t800, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 800, 0.64).
narrative_ontology:measurement_basis(gita_be_t800, projected).
narrative_ontology:measurement(gita_be_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1200, 0.67).
narrative_ontology:measurement_basis(gita_be_t1200, projected).
narrative_ontology:measurement(gita_be_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1600, 0.69).
narrative_ontology:measurement_basis(gita_be_t1600, observed).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.71).
narrative_ontology:measurement_basis(gita_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(gita_su_t0, projected).
narrative_ontology:measurement(gita_su_t400, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 400, 0.72).
narrative_ontology:measurement_basis(gita_su_t400, projected).
narrative_ontology:measurement(gita_su_t800, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 800, 0.74).
narrative_ontology:measurement_basis(gita_su_t800, projected).
narrative_ontology:measurement(gita_su_t1200, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1200, 0.76).
narrative_ontology:measurement_basis(gita_su_t1200, projected).
narrative_ontology:measurement(gita_su_t1600, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1600, 0.77).
narrative_ontology:measurement_basis(gita_su_t1600, observed).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement_basis(gita_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gita_kurukshetra_discourse__orthodox_literal_reading, 0.08).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the gita_kurukshetra_discourse kernel. orthodox_literal_reading authors caste hierarchy and righteous-war violence as structurally mandated and extractive (tangled_rope, epsilon 0.71). gandhian_allegorical_reading authors the same text with the battlefield as internal-struggle metaphor and no literal caste mandate (expected near-mountain/rope, epsilon near-negligible). universalist_devotional_reading authors bhakti as caste-independent, directly denying the beneficiary structure this reading declares (expected rope, low epsilon). All three share the textual kernel but diverge sharply in epsilon and beneficiary/victim structure — per the epsilon-invariance principle, they are authored as three separate constraint stories linked here, not as one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
