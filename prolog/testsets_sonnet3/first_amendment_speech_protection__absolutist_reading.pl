% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__absolutist_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__absolutist_reading
 *   human_readable: First Amendment Absolutist Reading (Categorical Text Literalism)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the First Amendment
 *   speech kernel: the text 'Congress shall make no law... abridging the
 *   freedom of speech' is read as categorical, with protection near-total
 *   except for a narrow, historically fixed list of exceptions (incitement to
 *   imminent lawless action, true threats, obscenity as narrowly defined,
 *   fighting words as narrowly construed, and a handful of others). Under
 *   this reading the exception list does not expand to accommodate new
 *   categories of harm — accumulated dignitary injury, coordinated online
 *   harassment, or group-targeted hate speech that falls short of incitement
 *   or true threats remains protected. This is a distinct constraint from the
 *   harm_limited_reading (which would treat demonstrable unconsented harm as
 *   itself exception-triggering) and the categorical_balancing_reading (which
 *   would weigh speech value against harm case by case). Each reading has a
 *   different ε, a different beneficiary/victim structure, and a different
 *   classification; they are linked only through network.affects_constraints
 *   and cs_structure.reading_relations, never merged.
 *
 * KEY AGENTS:
 *   - dominant_speakers: primary beneficiary — maximal protected latitude
 *   - majoritarian_political_actors: beneficiary and agenda-adjacent — protected from future counter-majoritarian speech regulation
 *   - media_and_publishing_institutions: institutional beneficiary — reduced compliance and liability exposure
 *   - targeted_racial_and_religious_minorities: primary payer — bears externalized harm from speech outside the narrow exceptions
 *   - harassment_targets_in_public_and_digital_fora: payer — self-censorship as the only practical remedy
 *   - federal_and_state_courts: agenda_setter — administers and could in principle revise the exception categories
 *   - civil_rights_litigators_and_scholars: excluded — arguments for expanding harm-based exceptions structurally denied controlling weight
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, 0.58).
domain_priors:suppression_score(first_amendment_speech_protection__absolutist_reading, 0.35).
domain_priors:theater_ratio(first_amendment_speech_protection__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__absolutist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__absolutist_reading, "First Amendment Absolutist Reading (Categorical Text Literalism)").
narrative_ontology:topic_domain(first_amendment_speech_protection__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__absolutist_reading, 'bc05f744-9f8e-446f-89ed-8ebdf526af9a').
narrative_ontology:cs_kernel_codification('bc05f744-9f8e-446f-89ed-8ebdf526af9a', fixed_text).
narrative_ontology:cs_authority_grounding('bc05f744-9f8e-446f-89ed-8ebdf526af9a', lineage).
narrative_ontology:cs_interpretation_layer_present('bc05f744-9f8e-446f-89ed-8ebdf526af9a').
narrative_ontology:cs_reading_relation('bc05f744-9f8e-446f-89ed-8ebdf526af9a', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc05f744-9f8e-446f-89ed-8ebdf526af9a', first_amendment_speech_protection__categorical_balancing_reading, influences).
narrative_ontology:cs_axiom('bc05f744-9f8e-446f-89ed-8ebdf526af9a', foundational, protected_speech_set_is_closed_and_historically_fixed).
narrative_ontology:cs_axiom_status(protected_speech_set_is_closed_and_historically_fixed, holdable).
narrative_ontology:cs_axiom_grounding('bc05f744-9f8e-446f-89ed-8ebdf526af9a', protected_speech_set_is_closed_and_historically_fixed, conventional).
narrative_ontology:cs_axiom('bc05f744-9f8e-446f-89ed-8ebdf526af9a', secondary, counter_speech_is_sufficient_remedy_for_uncaptured_harm).
narrative_ontology:cs_axiom_status(counter_speech_is_sufficient_remedy_for_uncaptured_harm, holdable).
narrative_ontology:cs_axiom_grounding('bc05f744-9f8e-446f-89ed-8ebdf526af9a', counter_speech_is_sufficient_remedy_for_uncaptured_harm, instrumental).
narrative_ontology:cs_reference_frame('bc05f744-9f8e-446f-89ed-8ebdf526af9a', founding_era_common_law_speech_categories).
narrative_ontology:cs_drift_state('bc05f744-9f8e-446f-89ed-8ebdf526af9a', contemporary_digital_harassment_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc05f744-9f8e-446f-89ed-8ebdf526af9a', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, dominant_speakers).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, majoritarian_political_actors).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__absolutist_reading, media_and_publishing_institutions).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, targeted_racial_and_religious_minorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__absolutist_reading, harassment_targets_in_public_and_digital_fora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy near-total legal cover to speak, publish, organize, and provoke without fear of civil or criminal liability for the content of speech itself. The absolutist reading maximizes what falls inside the protected set, so anyone already positioned to speak loudly, publish widely, or organize openly gets the benefit of that expanded set at essentially no cost to themselves.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, dominant_speakers, beneficiary,
    powerful, civilizational, arbitrage, national).

% Political majorities and their representatives benefit from a doctrine that treats content-based restriction as almost per se illegitimate, because it protects their own majoritarian speech and organizing from future counter-majoritarian regulation, and because courts applying this reading are appointed and confirmed through majoritarian political processes.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, majoritarian_political_actors, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__absolutist_reading, majoritarian_political_actors, agenda_setter).

% Media companies, platforms, and publishers rely on the categorical reading to avoid liability for content decisions and to resist regulatory intervention framed as content-based. They lobby to preserve and extend the absolutist frame because it minimizes their own compliance costs and legal exposure.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, media_and_publishing_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Bear the accumulated social, psychological, and material cost of speech the absolutist reading refuses to treat as an exception-triggering harm — organized hate speech, targeted intimidation short of the narrow historical exceptions (incitement, true threats, fighting words as construed narrowly). They cannot exit the jurisdiction whose courts hold this reading, and their only recourse is counter-speech, which the doctrine treats as the sufficient remedy regardless of actual power asymmetry.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, targeted_racial_and_religious_minorities, payer,
    powerless, generational, trapped, national).

% Individuals subject to sustained, coordinated harassment campaigns that fall short of the narrow categorical exceptions (true threats, incitement) absorb the cost as a matter of doctrine. Their practical exit is self-censorship or withdrawal from public and digital participation — the doctrine calls this a private cost, not a First Amendment harm.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, harassment_targets_in_public_and_digital_fora, payer,
    powerless, biographical, constrained, national).

% Advocate for recognizing accumulated dignitary and equality harms as within the categorical exceptions or as a basis for a different doctrinal frame; their arguments are structurally excluded from controlling weight under the absolutist reading, which treats the exception list as closed and historically fixed rather than open to harm-based expansion.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, civil_rights_litigators_and_scholars, excluded,
    organized, generational, constrained, national).

% Adjudicate what falls inside the narrow historical exception categories and enforce the categorical wall against content-based regulation. They administer the doctrine and could, in principle, expand or contract the exception list, but stare decisis and the text's own 'no law' language create strong doctrinal inertia toward the absolutist frame.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, federal_and_state_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyze the doctrine's internal coherence, historical grounding, and distributive consequences without being direct parties to litigation; supply competing genealogies of the text's original meaning and its exception categories.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__absolutist_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__absolutist_reading, dominant_speakers).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable, content-neutral rule courts and legislators can apply without case-by-case weighing of speech value against harm, reducing the risk that state power is used to suppress dissenting, unpopular, or minority political speech.
% TRANSFER_FUNCTION: Moves the cost of unregulated harmful speech from the state (which would otherwise bear the cost of adjudicating and restricting it) onto the individuals and groups targeted by that speech, while the benefit of maximal expressive latitude accrues to speakers generally and disproportionately to those already positioned to speak with reach and force.
% ABSENT_VOICES: Historically targeted minority communities and coordinated-harassment victims are structurally underrepresented in the doctrine's formation; the doctrine developed substantially through cases where majority-group speakers challenged restriction, not through direct input from those bearing the externalized harm, who would argue the exception categories are drawn too narrowly and too historically frozen.
% DISAPPEARANCE_RATIONALE: If the absolutist reading disappeared and courts adopted a harm-balancing or harm-limited frame instead, legislatures could regulate categories of speech currently untouchable (organized harassment campaigns, certain hate speech, some forms of disinformation), civil liability for reputational and dignitary harm from speech would expand, and speakers currently protected by the categorical wall would face new compliance costs and litigation risk — the practical scope of permissible public speech would visibly narrow.
% FOUNDING_PROBLEM: The founding problem was state suppression of political and religious dissent — licensing regimes, seditious libel prosecutions, and majoritarian silencing of minority viewpoints that the framers had directly experienced under British and early American practice.
% FOUNDING_PROBLEM_CORROBORATION: Free-speech advocacy organizations and originalist scholars attest the founding problem (state suppression of dissent) remains fully live and cite ongoing government efforts to regulate disfavored speech as proof. Civil rights scholars and equality-focused legal scholars, writing from outside the beneficiary set, attest that the founding problem has been substantially addressed by other doctrinal and political developments (broad press freedom, robust political competition) while the absolutist reading now chiefly functions to shield harm-causing speech that has little connection to the founders' concern with state suppression of political dissent.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(first_amendment_speech_protection__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__absolutist_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58 at interval end) is authored moderate-high, not extreme: the doctrine genuinely does perform a coordination function (preventing state suppression of political dissent), but under this specific reading the coordination benefit is captured disproportionately by already-powerful speakers while the cost of the doctrine's refusal to expand its exception categories falls on identifiable, less powerful groups. Suppression is authored comparatively low (0.35) because the mechanism here is not coercive silencing of the payers — it is the opposite: an absence of legal remedy, not an active gag. What looks like a mountain (settled constitutional text, judicial enforcement, apparent naturalness) is authored as tangled_rope because the coordination function is real AND the extraction is real and structurally asymmetric, requiring active judicial enforcement of the categorical wall against expansion.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (federal/state courts) and the beneficiary seats, the doctrine reads as principled protection of a coordination good — free political discourse — administered even-handedly. From the payer seats, the same doctrine reads as a structure that consistently declines to recognize their harm as cognizable, producing a stable asymmetry between who is protected in the exercise of speech and who absorbs its costs. The engine computes these as different seat-level classifications from the same structural data; this divergence is exactly what a tangled_rope reading is meant to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant speakers, majoritarian political actors, and media institutions are declared beneficiaries: they exercise the protected latitude and bear essentially none of the externalized cost, so directionality sits near the full-beneficiary end. Targeted minorities and harassment targets are declared victims with trapped/constrained exit — they cannot litigate their way out of the doctrine and cannot exit the jurisdiction, so directionality sits near the full-target end. Civil rights litigators are excluded rather than victimized directly — their exclusion is from the doctrine-shaping conversation itself, which is a distinct structural position captured by the excluded role and the absent_voices field, not by victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state suppression of political and religious dissent) is genuinely still live in some registers — the corroboration entry documents ongoing state efforts to regulate disfavored speech, which free-speech advocates cite as proof the doctrine's original function persists. But the founding_problem_status is authored contested, not dead, because the doctrine's original target (state censorship of minority political viewpoints) has been substantially supplemented by a different practical function today: shielding speech that harms private, often already-marginalized targets, with no connection to state censorship of political dissent. Classifying this as tangled_rope rather than mountain or pure snare prevents both errors: it does not treat the doctrine as inert natural law immune to distributive critique, and it does not treat the doctrine as pure extraction with no genuine coordination value, since the anti-censorship function is real and worth preserving in some form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_kernel_scope_ambiguity,
    'Does the First Amendment''s ''no law'' text, read in its 18th-century historical context, actually license a categorical exception list frozen to founding-era categories, or does the text''s own open-endedness (''the freedom of speech'' as a preexisting common-law concept) permit exception-category evolution while remaining textually absolutist?',
    'Historical-linguistic analysis of founding-era common law free speech doctrine (which already recognized categories like defamation and incitement) compared against the modern doctrine''s closed-list treatment; scholarly consensus on whether ''the freedom of speech'' was understood as a fixed catalog or an evolving common-law concept at ratification.',
    'If the founding-era concept was already evolving/common-law in character, the absolutist reading''s claim to textual fidelity weakens substantially, and the categorical_balancing_reading gains genealogical support. If the founding-era concept was genuinely a fixed catalog, the absolutist reading''s textualist claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_kernel_scope_ambiguity, conceptual, 'Whether the absolutist reading''s claimed textual fidelity survives close historical analysis of the founding-era speech concept.').

omega_variable(
    committer_structure_reading_disagreement_location,
    'This constraint is one reading (absolutist_reading) of the first_amendment_speech_protection kernel. The sibling readings (harm_limited_reading, categorical_balancing_reading) locate the disagreement differently: harm_limited_reading disputes where the harm threshold sits (any demonstrable unconsented harm vs. only the narrow historical categories); categorical_balancing_reading disputes the METHOD (case-by-case weighing vs. fixed categorical rule). Where exactly is the disagreement located structurally?',
    'Doctrinal mapping of the three readings'' decision procedures side by side: absolutist_reading uses a closed category test; categorical_balancing_reading uses an open weighing test; harm_limited_reading uses a harm-threshold test that could be closed or open depending on how ''demonstrable unconsented harm'' is specified. The disagreement is simultaneously about content (what counts as protected) and method (how the boundary is drawn), which is why they cannot be merged into one constraint.',
    'A sibling reading adopting harm_limited_reading''s harm-threshold test would recognize significantly more of the currently-externalized costs borne by targeted_racial_and_religious_minorities and harassment_targets_in_public_and_digital_fora as legally cognizable, shrinking this reading''s beneficiary capture. A shift to categorical_balancing_reading would preserve some speaker protection while allowing incremental harm recognition through weighing rather than either bright-line rule.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_reading_disagreement_location, conceptual, 'Locating the structural disagreement among the three kernel readings: content (harm threshold) vs. method (categorical rule vs. balancing).').

omega_variable(
    empirical_harm_magnitude_of_uncaptured_speech,
    'How large, in practice, is the category of speech that causes serious, demonstrable harm to targeted minorities but falls outside the absolutist reading''s narrow historical exceptions (incitement, true threats, narrowly-construed fighting words)?',
    'Empirical studies quantifying documented psychological, economic, and physical-safety harms from organized hate speech and coordinated harassment campaigns that were litigated and lost under current doctrine, compared against harms from speech that WAS successfully restricted under the existing exceptions.',
    'A large uncaptured-harm category would support reclassifying this constraint''s extractiveness upward and would strengthen the case for the harm_limited_reading; a small uncaptured-harm category would suggest the absolutist reading''s cost is more theoretical than material.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_harm_magnitude_of_uncaptured_speech, empirical, 'The empirical size of harm currently externalized by the absolutist reading''s narrow exception list.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__absolutist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__absolutist_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__absolutist_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__absolutist_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__absolutist_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__absolutist_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__absolutist_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__absolutist_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 10, 0.29).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 30, 0.31).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 50, 0.34).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__absolutist_reading, suppression_requirement, 60, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__harm_limited_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__absolutist_reading, first_amendment_speech_protection__categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language 'First Amendment free speech clause' per the epsilon-invariance principle: absolutist_reading (this file, ε=0.58, tangled_rope), harm_limited_reading, and categorical_balancing_reading. Each reading has a distinct exception-boundary test, a distinct beneficiary/victim structure, and its own stable ε — they are not the same constraint measured three ways but three structurally distinct constraints sharing a contested textual kernel. Linked bidirectionally via network.affects_constraints and via cs_structure.reading_relations in each file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
