% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3, Negative Liberty Reading: Prohibition on State Deprivation of Life and Liberty
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the negative_liberty_reading of the
 *   udhr_article_3 kernel ('Everyone has the right to life, liberty and
 *   security of person', UDHR 1948, elaborated in ICCPR Arts. 6 and 9): the
 *   article operates as a prohibition on state deprivation of life and
 *   liberty, exceptions only through narrow procedural justice, and
 *   'security' means freedom from state violence. The parties: individuals
 *   gain an enforceable guarantee against state killing and arbitrary
 *   detention; the state's collective security apparatus bears the cost —
 *   capital punishment abolition, restricted self-defense justifications,
 *   detention limits, expansive procedure; constitutional and human-rights
 *   courts administer and progressively expand the reading's content. The
 *   kernel contest is live and the family is decomposed per the
 *   epsilon-invariance principle:
 *   udhr_article_3__positive_entitlement_reading reads the same text as an
 *   obligation of state provision (epsilon re-referenced onto material
 *   provision; the state flips toward obligated provider), and
 *   udhr_article_3__procedural_hybrid_reading guarantees procedure while
 *   deferring the substantive contest (epsilon near coordination cost). This
 *   story's epsilon (0.72) is high because the reading, as enforced, demands
 *   abolition and restructuring of existing security practice. The operative
 *   constraint runs through constitutional entrenchment and treaty
 *   incorporation — the Declaration itself carries no enforcement machinery;
 *   the measurement series tracks the incorporated constraint's operation
 *   1948-2024 (t=0 to t=76). Claim and metrics are authored independently:
 *   claimed_type tangled_rope is my structural judgment (genuine
 *   founding-level coordination plus concentrated cost on the security
 *   apparatus through the same structure); the metrics describe the reading's
 *   actual operation.
 *
 * KEY AGENTS:
 *   - individual_rights_holders: Primary beneficiary (powerless/constrained) — holds the enforceable guarantee against state killing and arbitrary detention; diffuse, unorganized, present in every jurisdiction the reading reaches
 *   - criminal_defendants: Direct beneficiary (powerless/trapped) — the narrow-procedural-justice gate runs to them at the moment of state contact; the reading's due-process demands are their protection
 *   - state_collective_security_apparatus: Primary payer (institutional/constrained) — police, detention, and emergency-power institutions bear abolition of capital punishment, restricted self-defense doctrine, and procedural limits on deprivation
 *   - retentionist_capital_punishment_states: Heaviest payer (institutional/constrained, secondary agenda-setter) — sovereigns whose core security practice the reading abolishes; they contest interpretation from inside the treaty system they could only exit at legitimacy collapse
 *   - constitutional_courts: Agenda-setter (institutional/constrained) — domestic constitutional courts and regional human-rights courts administer the reading, set its content through jurisprudence, and collect interpretive authority as it expands
 *   - un_human_rights_committee: Agenda-setter (institutional/constrained) — treaty body administering state review and issuing general comments that thicken the reading's content
 *   - human_rights_advocacy_organizations: Beneficiary (organized/mobile) — litigate, report, and fundraise on the arrangement's operation; collect standing and mission-relevance without running it
 *   - victims_of_private_violence: Excluded (powerless/trapped) — the diffuse class whose claimed protection by the restricted measures has no seat in constitutional litigation; their interest is spoken for only by the security apparatus the reading binds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.72).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3, Negative Liberty Reading: Prohibition on State Deprivation of Life and Liberty").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '86de9e04-619a-4e0c-8789-2a88b0a076fe').
narrative_ontology:cs_kernel_codification('86de9e04-619a-4e0c-8789-2a88b0a076fe', fixed_text).
narrative_ontology:cs_authority_grounding('86de9e04-619a-4e0c-8789-2a88b0a076fe', lineage).
narrative_ontology:cs_interpretation_layer_present('86de9e04-619a-4e0c-8789-2a88b0a076fe').
narrative_ontology:cs_reading_relation('86de9e04-619a-4e0c-8789-2a88b0a076fe', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('86de9e04-619a-4e0c-8789-2a88b0a076fe', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('86de9e04-619a-4e0c-8789-2a88b0a076fe', foundational, security_is_freedom_from_state_violence).
narrative_ontology:cs_axiom_status(security_is_freedom_from_state_violence, holdable).
narrative_ontology:cs_axiom_grounding('86de9e04-619a-4e0c-8789-2a88b0a076fe', security_is_freedom_from_state_violence, deontological).
narrative_ontology:cs_axiom('86de9e04-619a-4e0c-8789-2a88b0a076fe', foundational, narrow_procedural_justice_sole_exception).
narrative_ontology:cs_axiom_status(narrow_procedural_justice_sole_exception, holdable).
narrative_ontology:cs_axiom_grounding('86de9e04-619a-4e0c-8789-2a88b0a076fe', narrow_procedural_justice_sole_exception, conventional).
narrative_ontology:cs_axiom('86de9e04-619a-4e0c-8789-2a88b0a076fe', secondary, capital_punishment_incompatible_with_article_3).
narrative_ontology:cs_axiom_status(capital_punishment_incompatible_with_article_3, holdable).
narrative_ontology:cs_axiom_grounding('86de9e04-619a-4e0c-8789-2a88b0a076fe', capital_punishment_incompatible_with_article_3, deontological).
narrative_ontology:cs_reference_frame('86de9e04-619a-4e0c-8789-2a88b0a076fe', negative_prohibition_on_state_deprivation).
narrative_ontology:cs_drift_state('86de9e04-619a-4e0c-8789-2a88b0a076fe', post_positive_obligations_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('86de9e04-619a-4e0c-8789-2a88b0a076fe', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individual_rights_holders).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, human_rights_advocacy_organizations).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_collective_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, retentionist_capital_punishment_states).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, narrow_procedural_justice_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__negative_liberty_reading, individual_inviolability_against_state_power).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every person within reach of the incorporated guarantee. They hold an enforceable claim that the state will not kill or imprison them outside narrow procedure; the guarantee follows them through near-universal treaty reach even if they emigrate. They pay for the enforcement machinery as taxpayers and bear any residual risk from restricted security measures, but the protection they collect is the arrangement's direct product. They are diffuse and unorganized; their seat is exercised through the courts that others litigate in.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individual_rights_holders, beneficiary,
    powerless, biographical, constrained, universal).

% Persons facing state prosecution or detention at the moment the guarantee applies most concretely. The narrow-procedural-justice gate runs to them: charge, counsel, hearing, review. They cannot exit their situation — the state holds them — so the guarantee's strength is their entire protection. Their cases are the raw material from which the courts thicken the reading's content.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, criminal_defendants, beneficiary,
    powerless, immediate, trapped, national).

% Police services, detention administrations, intelligence and emergency-power institutions. They bear the reading's operating costs: abolition of capital punishment where they used it, restricted self-defense and emergency justifications, procedural friction on every deprivation of liberty, and the litigation burden of defending their practices. They cannot abandon their security function without dissolving themselves, and their objection to the reading is permanent but channeled through the very courts that bind them.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_collective_security_apparatus, payer,
    institutional, generational, constrained, national).

% Sovereigns that retain the death penalty and face the reading's heaviest demand: abolition. They ratified the treaty system that now constrains them and participate in its review bodies, so they contest the reading from inside the arrangement they could only exit at the cost of legitimacy collapse — denunciation marks a state as a rights violator and isolates it. They are simultaneously the arrangement's most burdened participants and among its authors.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, retentionist_capital_punishment_states, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, retentionist_capital_punishment_states, agenda_setter).

% Domestic constitutional courts and regional human-rights courts. They administer the guarantee: they decide what counts as narrow procedural justice, strike down practices that fail the gate, and progressively thicken the reading's content through doctrine. Their authority and docket grow with the reading's reach; they are bound by text, precedent, and jurisdiction and cannot exit their function.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, continental).

% The treaty body that reviews state compliance, hears individual communications, and issues general comments defining the guarantee's content between amendments. It has no enforcement arm of its own — its product is interpretation and reputational pressure — but its interpretations are the raw material domestic courts cite. It is bound to the treaty system and cannot exit it.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, un_human_rights_committee, agenda_setter,
    institutional, generational, constrained, global).

% NGOs and litigating foundations that bring the test cases, document violations, and fund the enforcement docket. They collect standing, mission-relevance, and funding from the arrangement's operation without running it; their operational base is mobile across jurisdictions and they exit a closing space by relocating.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, human_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% The diffuse class of people exposed to private violence who claim — through the security apparatus, since they have no voice of their own in the litigation — that the restricted measures (capital punishment's deterrence, preventive detention's incapacitation) would have protected them. They are unorganized and absent; their claimed protection is the subject of the empirical contest the omegas track, not a settled fact.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, victims_of_private_violence, excluded,
    powerless, biographical, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, individual_rights_holders).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founding collective-action problem of political society: maintaining a state strong enough to secure order without leaving individuals hostage to its discretionary violence. The reading coordinates everyone's expectations on a mutual guarantee — no one is killed or imprisoned by the state except through narrow, auditable procedure — converting state coercive power from a standing predator-risk into a predictable, contestable institution. It is the coordination layer that makes consent to state authority rational for the governed.
% TRANSFER_FUNCTION: Moves security discretion from state institutions to individuals: the state surrenders capital punishment, preventive detention, summary procedure, and broad self-defense justifications; individuals receive an enforceable guarantee of bodily and liberty security. The flow runs from concentrated state coercive capacity to diffuse individual protection, carried by courts whose operating costs the state also bears.
% ABSENT_VOICES: The diffuse class of potential victims of private violence who claim protection from the restricted measures — capital punishment's deterrent effect, preventive detention's incapacitation — has no seat: they are unorganized, their interest is spoken for only by the security apparatus the reading binds (which discredits it), and they appear in constitutional litigation only as intervenors in others' cases. Retentionist states and the security apparatus object loudly but are present as losing litigants; future victims of residual crime risk are the voice actually missing.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, retentionist states would resume executions, security services would expand preventive detention and emergency powers, and the due-process architecture (habeas, exclusion of coerced evidence, narrow self-defense doctrine) would erode within a decade; individuals' security would revert from an enforceable guarantee to a revocable state-granted condition, and the constitutional orders built on procedural legitimacy would rearrange around whatever each sovereign chose to tolerate.
% FOUNDING_PROBLEM: The drafters of 1948 had watched a civilized legal order authorize atrocity: emergency decrees, summary execution, administrative detention, all through law. Article 3's negative reading was built to make individual life and liberty a limit on state action rather than a state-granted privilege — so that no future legal order could convert its monopoly of violence into an instrument of extermination while claiming legality.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the UN Human Rights Committee's General Comment 36 (2018) documents continuing extrajudicial killing, enforced disappearance, and arbitrary detention across jurisdictions; OHCHR casualty and detention reporting attests the persistence of state violence; the drafting-history record (the 1947 drafters' consultations warning that the individual needed protection against the state, not only against private wrongs) is confirmed by independent legal-historical scholarship. No attestation from the benefiting parties is relied on.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is high (0.72) because the reading's enforced content removes substantial state security capacity: capital punishment abolition (the heaviest single demand, binding on retentionist sovereigns), restrictive self-defense doctrine, detention limits, and expansive due process — and the demands intensified monotonically across the interval as courts thickened the reading (0.25 at the unenforced 1948 declaration to 0.72 today). Suppression (0.68) is authored as the constraint's raw structural property — the enforcement coercion applied to states — and is NOT scaled by scope or power; only extractiveness is scaled downstream by directionality and scope in the engine's computation. The suppression series tracks the enforcement machinery built to hold the constraint against state preference: constitutional review, individual petition, treaty-body review — maturing rapidly mid-interval, then plateauing, with states retaining only costly exits (entrenched amendment, denunciation at legitimacy collapse). Theater (0.25, falling from 0.45) reflects the shift from ceremonial declaration-era performance to functional enforcement, with residual ritualism in treaty reporting and ratification-with-reservation. Accessibility collapse (0.45) is low-moderate because the alternatives have not collapsed: the sibling readings are live, retention persists in defiance in some ICCPR parties, and sovereigntist arrangements persist outside the treaty system. Resistance (0.60) is concentrated entirely in the paying seats — retentionist defiance, security-services litigation, law-and-order politics — while the beneficiary seats mount none. All three metric series share one time grid (eight points, t=0 to t=76), so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the measurement. From the individual and defendant seats, the constraint operates as pure protection — a guarantee they collect without paying, with directionality near the beneficiary end. From the security-apparatus and retentionist-state seats, the same structure operates as heavy removal of capacity they did not consent to surrender, with directionality near the target end. From the courts' seat, the constraint is the constitutive function they administer; they collect interpretive authority as it expands, a mild beneficiary lean the power-atom fallback does not capture (noted here; no override authored, because a power-atom-level correction would misfire across institutional seats — the apparatus and retentionist states share the 'institutional' atom but sit at the opposite end). Among same-level sovereign seats the experience diverges by prior compliance: abolitionist states bear near-zero marginal cost and collect legitimacy, while retentionist states bear the full abolition demand — same power atom, opposite structural positions. My claimed_type (tangled_rope) adjudicates the whole structure, not any seat: the engine's per-seat classifications should diverge exactly along this line. The paying seats also constitute a potential coalition (law-and-order politics, coordinated treaty resistance), which is why their resistance contribution dominates the scalar despite their institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (individual_rights_holders, criminal_defendants, human_rights_advocacy_organizations) drive directionality toward the beneficiary end for the protected seats; victim declarations (state_collective_security_apparatus, retentionist_capital_punishment_states) drive it toward the target end for the paying seats. Exit structure modulates within the ends: defendants are trapped (in state custody at the constraint's moment of application), rights holders are constrained (emigration is possible but the guarantee follows them through near-universal treaty reach), the apparatus is constrained (it cannot abandon its security function without dissolving itself), and retentionist states are constrained rather than mobile because denunciation carries legitimacy collapse — the exits actually taken (treaty withdrawal) have been rare and ruinous. The courts and the treaty body carry no beneficiary/victim declaration and fall back on their power atom — approximately symmetric, which slightly understates their authority accrual; the structural data, not an override, remains the honest input, and no directionality_overrides are authored because the derivation chain already produces the right relationships from the declared beneficiaries, victims, and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification does double preventive work. Against mislabeling as pure coordination: the cost side is real and concentrated — the security apparatus and retentionist states lose core practice through the same structure that protects everyone, and the excluded voice (potential victims of private violence) bears a diffuse cost with no seat; a pure-coordination framing would hide that asymmetry behind the diffuse benefit. Against mislabeling as pure extraction: the coordination function is genuine and primary — mutual security against state violence is the founding coordination problem of constitutional order, not cover for it — and the beneficiaries are net-protected, not fleeced. The mandate-migration omega tracks the residual lifecycle risk: if the reading's enforcement energy has migrated from atrocity-prevention (its founding problem, still live and externally corroborated) to ordinary criminal-justice policy, the constraint's persistence would need the degraded-function apparatus rather than the hybrid one; current theater (0.25) and the live founding problem keep that hypothesis dormant. The R5 interview records the founding problem as live with corroboration from outside the beneficiary set, so no capture/zombie mismatch is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading (negative_liberty_reading) of the udhr_article_3 kernel; is it the correct instantiation, or do the sibling readings better capture what ''life, liberty and security of person'' binds states to? The disagreement is located in the text''s term ''security'' (freedom from state violence versus freedom from material insecurity) and in the article''s function (prohibition versus obligation versus procedure).',
    'Comparative constitutional jurisprudence across jurisdictions that have adopted each reading; drafting history (1946-1948 travaux preparatoires) on the drafters'' committed meaning of ''security''; stability analysis of which reading''s constraint survives judicial application without requiring the text to mean something its drafters did not commit to.',
    'Adopting udhr_article_3__positive_entitlement_reading inverts the beneficiary/victim structure (the state becomes the obligated provider, those denied material conditions become the paying seat) and re-references epsilon onto welfare provision; adopting udhr_article_3__procedural_hybrid_reading strips the substantive contest, drops epsilon toward procedural-coordination levels, and removes capital-punishment abolition as an epsilon driver.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the Article 3 kernel this constraint instantiates, and what each sibling reading would change structurally.').

omega_variable(
    security_capacity_vs_rent_question,
    'Is the reading''s heavy cost to the collective security apparatus (capital punishment abolition, restricted self-defense doctrine, detention limits) the removal of genuine protective capacity, or the stripping of surplus capacity the apparatus would not need under narrow procedural justice?',
    'Cross-jurisdictional comparison of public-safety outcomes between retentionist and abolitionist, procedurally-narrow and procedurally-expansive regimes, controlling for demographics and reporting; cost-structure analysis of what the restricted measures actually deliver at the margin.',
    'If the stripped capacity is surplus, the constraint sits closer to pure coordination and the paying seats'' extraction experience is overstated; if genuine capacity is removed, the coordination/extraction hybrid is confirmed and the excluded voice''s claim gains standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_capacity_vs_rent_question, empirical, 'Whether the constraint''s cost to collective security is surplus removal or genuine protective capacity destroyed.').

omega_variable(
    restricted_measures_protective_value,
    'Do the measures this reading abolishes or restricts — capital punishment, preventive detention, broad state self-defense — actually protect potential victims of private violence at the margin the excluded voice claims?',
    'Deterrence and incapacitation research (the National Research Council''s 2012 capital-punishment review found deterrence evidence inconclusive); natural experiments from abolition episodes tracking homicide and recidivism of the affected offender classes.',
    'Near-zero protective value weakens the victim status of collective security measures and moves the constraint toward pure coordination; substantial protective value makes the diffuse excluded victims a real cost-bearing class the current stakeholder surface underweights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restricted_measures_protective_value, empirical, 'Whether the restricted security measures have real protective value for the absent voice.').

omega_variable(
    mandate_migration_question,
    'Has the reading''s operative mandate migrated from its founding problem (preventing legal authorization of atrocity) to ordinary criminal-justice policy (capital punishment abolition, due-process expansion in ordinary cases)?',
    'Track the reading''s enforcement energy: the share of Article 3-family litigation addressing atrocity-scale state violence versus ordinary criminal procedure, and doctrinal citation patterns in leading constitutional and human-rights courts.',
    'If migrated, the constraint persists on a new mandate and lifecycle-decay analysis applies rather than the founding-problem justification; if not migrated, the founding problem remains the binding function and the mandate is intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_migration_question, empirical, 'Whether the reading''s function has drifted from atrocity-prevention to ordinary criminal justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_article_3__negative_liberty_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(udhr_tr_t0, observed).
narrative_ontology:measurement(udhr_tr_t12, udhr_article_3__negative_liberty_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(udhr_tr_t12, observed).
narrative_ontology:measurement(udhr_tr_t24, udhr_article_3__negative_liberty_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement_basis(udhr_tr_t24, observed).
narrative_ontology:measurement(udhr_tr_t36, udhr_article_3__negative_liberty_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement_basis(udhr_tr_t36, observed).
narrative_ontology:measurement(udhr_tr_t48, udhr_article_3__negative_liberty_reading, theater_ratio, 48, 0.25).
narrative_ontology:measurement_basis(udhr_tr_t48, observed).
narrative_ontology:measurement(udhr_tr_t60, udhr_article_3__negative_liberty_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(udhr_tr_t60, observed).
narrative_ontology:measurement(udhr_tr_t68, udhr_article_3__negative_liberty_reading, theater_ratio, 68, 0.24).
narrative_ontology:measurement_basis(udhr_tr_t68, observed).
narrative_ontology:measurement(udhr_tr_t76, udhr_article_3__negative_liberty_reading, theater_ratio, 76, 0.25).
narrative_ontology:measurement_basis(udhr_tr_t76, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_article_3__negative_liberty_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(udhr_be_t0, observed).
narrative_ontology:measurement(udhr_be_t12, udhr_article_3__negative_liberty_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement_basis(udhr_be_t12, observed).
narrative_ontology:measurement(udhr_be_t24, udhr_article_3__negative_liberty_reading, base_extractiveness, 24, 0.41).
narrative_ontology:measurement_basis(udhr_be_t24, observed).
narrative_ontology:measurement(udhr_be_t36, udhr_article_3__negative_liberty_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement_basis(udhr_be_t36, observed).
narrative_ontology:measurement(udhr_be_t48, udhr_article_3__negative_liberty_reading, base_extractiveness, 48, 0.6).
narrative_ontology:measurement_basis(udhr_be_t48, observed).
narrative_ontology:measurement(udhr_be_t60, udhr_article_3__negative_liberty_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(udhr_be_t60, observed).
narrative_ontology:measurement(udhr_be_t68, udhr_article_3__negative_liberty_reading, base_extractiveness, 68, 0.7).
narrative_ontology:measurement_basis(udhr_be_t68, observed).
narrative_ontology:measurement(udhr_be_t76, udhr_article_3__negative_liberty_reading, base_extractiveness, 76, 0.72).
narrative_ontology:measurement_basis(udhr_be_t76, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_article_3__negative_liberty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(udhr_su_t0, observed).
narrative_ontology:measurement(udhr_su_t12, udhr_article_3__negative_liberty_reading, suppression_requirement, 12, 0.18).
narrative_ontology:measurement_basis(udhr_su_t12, observed).
narrative_ontology:measurement(udhr_su_t24, udhr_article_3__negative_liberty_reading, suppression_requirement, 24, 0.3).
narrative_ontology:measurement_basis(udhr_su_t24, observed).
narrative_ontology:measurement(udhr_su_t36, udhr_article_3__negative_liberty_reading, suppression_requirement, 36, 0.42).
narrative_ontology:measurement_basis(udhr_su_t36, observed).
narrative_ontology:measurement(udhr_su_t48, udhr_article_3__negative_liberty_reading, suppression_requirement, 48, 0.55).
narrative_ontology:measurement_basis(udhr_su_t48, observed).
narrative_ontology:measurement(udhr_su_t60, udhr_article_3__negative_liberty_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(udhr_su_t60, observed).
narrative_ontology:measurement(udhr_su_t68, udhr_article_3__negative_liberty_reading, suppression_requirement, 68, 0.66).
narrative_ontology:measurement_basis(udhr_su_t68, observed).
narrative_ontology:measurement(udhr_su_t76, udhr_article_3__negative_liberty_reading, suppression_requirement, 76, 0.68).
narrative_ontology:measurement_basis(udhr_su_t76, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Article 3 right to life, liberty and security' covers three structurally distinct claims that share one kernel text but diverge in epsilon, beneficiary/victim structure, and enforcement. This story authors the negative prohibition (epsilon high: the reading abolishes and restructures existing security practice; the paying seats are the collective security apparatus and retentionist states). udhr_article_3__positive_entitlement_reading authors the state-provision obligation (epsilon re-referenced onto material provision; the state flips from bound party toward obligated provider and the security apparatus flips toward beneficiary). udhr_article_3__procedural_hybrid_reading authors the procedural guarantee alone (epsilon near coordination cost; the substantive contest is deferred, so neither the apparatus nor any welfare claimant bears the reading's costs). The negative reading is upstream of the hybrid (its procedural apparatus is the hybrid's content) and competes with the positive reading for the text's meaning; all three are linked through network.affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
