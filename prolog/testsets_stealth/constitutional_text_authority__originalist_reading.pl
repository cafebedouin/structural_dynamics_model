% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__originalist_reading, []).

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
 *   constraint_id: constitutional_text_authority__originalist_reading
 *   human_readable: Originalist Fixity of Constitutional Meaning (Historical Public Understanding Authority)
 *   domain: legal/constitutional/interpretive
 *
 * SUMMARY:
 *   The constraint under classification is the originalist reading of the
 *   constitutional_text_authority kernel: constitutional meaning is fixed at
 *   ratification, and the text's authority derives from the historical public
 *   understanding of those who enacted it. As an operating arrangement it
 *   binds judges — they may not update meaning to contemporary values,
 *   permissible outcomes are gated by historical evidence,
 *   unenumerated-rights claims must find founding-era or reconstruction-era
 *   footing, and post-ratification social change reaches the Constitution
 *   only through Article V. The constraint is claimed here as tangled_rope:
 *   it performs a genuine coordination function (constraining judicial
 *   discretion, preserving democratic authorship of supreme law) while the
 *   same structure asymmetrically burdens unenumerated-rights claimants and
 *   groups whose founding-era exclusion left no protective tradition for them
 *   to invoke. Claim and metrics are authored independently: the metrics
 *   describe the arrangement's actual operation as this authoring seat sees
 *   it; the engine computes each seat's classification from the structural
 *   data. The ε referent is the fixed-meaning arrangement itself as it
 *   operates — never the living-constitutionalist arrangement its critics
 *   would install. Sibling readings (living_constitutionalist_reading,
 *   positivist_reading) are separate constraints, linked via
 *   network.affects_constraints, not part of this story.
 *
 * KEY AGENTS:
 *   - - supreme_court_originalist_majority: agenda-setter (institutional / identity_locked) — administers the fixed-meaning rule and decides which history counts
 *   - - state_governments: primary beneficiary (institutional / constrained) — collects federalism limits and defense wins against unenumerated-rights claims
 *   - - elected_legislatures: secondary beneficiary (institutional / constrained) — retains regulatory space and the Article V gate
 *   - - originalist_legal_movement: beneficiary and reproduction mechanism (organized / constrained) — appointment pipeline, gatekeeping, professional discipline
 *   - - unenumerated_rights_claimants: primary target (powerless / trapped) — claims gated on historical evidence they must fund
 *   - - historically_disenfranchised_groups: target (organized / trapped) — must invoke a protective tradition their own exclusion prevented from forming
 *   - - living_constitutionalist_judges: dissenting payer (institutional / trapped) — bound by a methodology they reject
 *   - - state_supreme_courts_rejecting_originalism: excluded (institutional / constrained) — develop independent state constitutional grounds outside the federal coalition
 *   - - professional_historians: observer (organized / mobile) — supply and contest the historical evidence the methodology consumes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, 0.58).
domain_priors:suppression_score(constitutional_text_authority__originalist_reading, 0.6).
domain_priors:theater_ratio(constitutional_text_authority__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(constitutional_text_authority__originalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__originalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__originalist_reading, "Originalist Fixity of Constitutional Meaning (Historical Public Understanding Authority)").
narrative_ontology:topic_domain(constitutional_text_authority__originalist_reading, "legal/constitutional/interpretive").

domain_priors:requires_active_enforcement(constitutional_text_authority__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__originalist_reading, 'd0403acf-e4eb-496e-b887-5a00e2e32282').
narrative_ontology:cs_kernel_codification('d0403acf-e4eb-496e-b887-5a00e2e32282', fixed_text).
narrative_ontology:cs_authority_grounding('d0403acf-e4eb-496e-b887-5a00e2e32282', lineage).
narrative_ontology:cs_interpretation_layer_present('d0403acf-e4eb-496e-b887-5a00e2e32282').
narrative_ontology:cs_reading_relation('d0403acf-e4eb-496e-b887-5a00e2e32282', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('d0403acf-e4eb-496e-b887-5a00e2e32282', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('d0403acf-e4eb-496e-b887-5a00e2e32282', foundational, popular_sovereignty_binds_later_generations).
narrative_ontology:cs_axiom_status(popular_sovereignty_binds_later_generations, holdable).
narrative_ontology:cs_axiom_grounding('d0403acf-e4eb-496e-b887-5a00e2e32282', popular_sovereignty_binds_later_generations, deontological).
narrative_ontology:cs_axiom('d0403acf-e4eb-496e-b887-5a00e2e32282', foundational, ratification_fixes_constitutional_meaning).
narrative_ontology:cs_axiom_status(ratification_fixes_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d0403acf-e4eb-496e-b887-5a00e2e32282', ratification_fixes_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('d0403acf-e4eb-496e-b887-5a00e2e32282', secondary, article_v_exclusive_revision_channel).
narrative_ontology:cs_axiom_status(article_v_exclusive_revision_channel, holdable).
narrative_ontology:cs_axiom_grounding('d0403acf-e4eb-496e-b887-5a00e2e32282', article_v_exclusive_revision_channel, conventional).
narrative_ontology:cs_axiom('d0403acf-e4eb-496e-b887-5a00e2e32282', secondary, drafters_subjective_intent_controls).
narrative_ontology:cs_axiom_status(drafters_subjective_intent_controls, overridden).
narrative_ontology:cs_axiom_grounding('d0403acf-e4eb-496e-b887-5a00e2e32282', drafters_subjective_intent_controls, conventional).
narrative_ontology:cs_reference_frame('d0403acf-e4eb-496e-b887-5a00e2e32282', ratification_era_public_understanding).
narrative_ontology:cs_drift_state('d0403acf-e4eb-496e-b887-5a00e2e32282', contemporary_law_office_history_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0403acf-e4eb-496e-b887-5a00e2e32282', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__originalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, state_governments).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, elected_legislatures).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, originalist_legal_movement).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, historically_disenfranchised_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__originalist_reading, professional_historians).
narrative_ontology:constraint_victim(constitutional_text_authority__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, judicial_restraint_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__originalist_reading, rule_of_law_predictability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Life-tenured justices, currently a majority, who decide which historical sources count, how much generality to permit, and whether a practice amounts to the tradition a claim requires. They author the opinions that apply the fixed-meaning rule and police the bench against interpretive deviation. Their exit from the methodology is effectively closed: their jurisprudential legacies, their nominations, and their professional standing are all built on it.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, supreme_court_originalist_majority, agenda_setter,
    institutional, generational, identity_locked, national).

% Defend their statutes against constitutional challenges and collect most of the wins when unenumerated-rights claims fail for want of historical footing. They also gain from originalist limits on federal power. Their alternatives — litigation strategy, state constitutional innovation, lobbying for amendments — all operate inside the same framework.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, state_governments, beneficiary,
    institutional, generational, constrained, national).

% Congress and the state legislatures retain regulatory space that evolving-rights adjudication would have narrowed, and retain the amendment power through which all constitutional change must now pass. They respond to adverse rulings by re-legislating or proposing amendments; they rarely need to leave the arrangement because it defers to them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, elected_legislatures, beneficiary,
    institutional, biographical, constrained, national).

% The network of scholars, judges, lawyers, and organizations that supplies the methodology's personnel and arguments: nomination screening, amicus networks, historical expertise, law-review infrastructure. It converts fidelity-to-ratification adjudication into appointments, clerkships, publications, and institutional influence, and it disciplines deviation through professional criticism.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, originalist_legal_movement, beneficiary,
    organized, biographical, constrained, national).

% Litigants claiming liberties the text does not name — bodily autonomy, privacy, family life. Under the fixed-meaning rule their claims must find footing in ratification- or reconstruction-era public understanding, which they must fund historians and archival research to establish, while governments need only defend. Their alternative channel, Article V amendment, is beyond their reach.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Groups — descendants of the enslaved, women in large part — whose equal civic status was not secured in the founding-era public understanding. The rule asks them to show a historical tradition of protecting the right they claim, but the political order that would have produced such a tradition excluded them from making it. They organize, litigate, and push amendments, but the record they must invoke was written without them.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, historically_disenfranchised_groups, payer,
    organized, generational, trapped, national).

% Judges and justices who hold that meaning evolves with social attitudes and who now sit in dissent or in the lower-court minority. They must author opinions within a methodology they reject or file dissents with no doctrinal effect. They cannot leave the bench and cannot install their methodology without personnel change.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, living_constitutionalist_judges, payer,
    institutional, generational, trapped, national).

% State high courts that have expressly declined to follow federal originalist method and instead develop independent state constitutional grounds for rights. They sit outside the federal interpretive coalition: their objections are heard but do not set the federal agenda, and their workarounds are confined to state constitutional text.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, state_supreme_courts_rejecting_originalism, excluded,
    institutional, generational, constrained, national).

% Academic historians of the founding and reconstruction eras who supply the evidence the methodology consumes — through amicus briefs, expert reports, and scholarship — and who publicly criticize judicial selectivity as law-office history. The methodology increased demand for their expertise; it also put them in an adversarial posture toward the judges who use their work.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__originalist_reading, professional_historians, observer,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__originalist_reading, professional_historians, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__originalist_reading, originalist_legal_movement).
narrative_ontology:fixing_cost_class(constitutional_text_authority__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constrains judicial discretion by supplying an external, determinate reference for constitutional meaning: the public understanding at ratification. This makes adjudication predictable, preserves the democratic authorship of supreme law, and channels constitutional change through Article V rather than judicial revision.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges to the ratification-era record and its professional interpreters; moves the cost of rights recognition onto claimants, who must fund the historical excavation their claims now require; moves litigation wins to governments defending statutes against unenumerated-rights challenges; moves appointments, clerkships, and scholarly authority to the originalist legal establishment.
% ABSENT_VOICES: The ratification-era public itself — enslaved people, women, and the unpropertied, whose 'understanding' the arrangement invokes but who could not vote, publish, or litigate — is structurally absent from the record treated as authoritative. Contemporary unenumerated-rights claimants appear as litigants, but their question — why must my liberty find a founding-era analogue? — is answered by the rule before they can ask it.
% DISAPPEARANCE_RATIONALE: If fixed-at-ratification meaning ceased to bind overnight, every doctrine built on history-and-tradition tests would be re-decidable, courts would need a new authority source (contemporary values or enactment pedigree), the appointment pipeline would lose its organizing principle, and the movement's gatekeeping role — amicus networks, historical expertise, nomination screening — would dissolve.
% FOUNDING_PROBLEM: The countermajoritarian difficulty: how can unelected judges bind a democratic polity to supreme law without becoming an unaccountable super-legislature? The originalist answer: judges apply the law the sovereign people enacted, and constitutional change returns to the people through Article V.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: living-constitutionalist scholars concede the countermajoritarian difficulty is real and propose rival answers to it; political scientists and comparative constitutional scholars independently document legitimacy anxiety as the driver of the originalist turn; the arrangement's sharpest academic critics attest the problem is live while rejecting the originalist solution. No party claims the legitimacy problem is solved — the parties dispute which reading solves it.
narrative_ontology:disappearance_verdict(constitutional_text_authority__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text_authority__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__originalist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: substantial but bounded. The arrangement forecloses unenumerated-rights claims that lack historical footing, imposes asymmetric litigation costs (claimants must fund historical excavation; governments need only defend), and locks in founding-era allocations; but it also binds governments and judges in clear-text cases, and channels change through a real if heavy amendment process. Suppression 0.60: enforcement is legitimacy-based rather than physical — confirmation politics, professional discipline via law-office-history accusations, movement gatekeeping of the bench, and the framing of rival methodology as illegitimate. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine. Theater_ratio 0.42: a documented share of judicial history is outcome-driven selection, but the methodology genuinely constrains in clear-text and federalism cases. Accessibility_collapse 0.52: within the methodology, alternatives partly collapse (a committed originalist cannot invoke contemporary values), but discretion survives in source selection, level of generality, and tradition framing, and the rival reading remains live in the profession. Resistance 0.65: sustained academic critique, dissents, state-court rejection, and political contestation. Measurements run on one shared grid (T0 ≈ 1980, originalism's consolidation as a self-conscious movement; T45 ≈ 2025); all three series rise: extraction grew as the arrangement moved from oppositional check on judicial discretion to dominant gate on rights recognition; suppression rose as enforcement machinery (appointment pipeline, professional discipline) matured; theater rose as stakes attracted outcome-driven historical argument. Suppression mechanism: predominantly structural (appointment pipeline, confirmation politics, professional gatekeeping) with an internalized component (justices' and clerks' professional identity fused with the methodology) — roughly 60/40; the identity-lock omega carries the residual uncertainty. Receipt surface: gains demonstrably accrue to the originalist_legal_movement seat, which converts the arrangement into appointments, clerkships, amicus gatekeeping, and scholarly authority — the continuous, structural capture; state and legislative gains are real but distributed across discrete case outcomes. Fixing is prohibitive for the seat that could fix it: removal requires reversing a life-tenured, identity-locked majority and dismantling the appointment pipeline, and Article V — the formal alternative — is prohibitively heavy by design.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (originalist majority) should compute a coordination-dominant type: from inside the methodology, the arrangement is fidelity to enacted law and the alternative is judicial supremacy. The trapped payer seats (unenumerated_rights_claimants, historically_disenfranchised_groups) should compute high effective extraction: the same historical gate that reads as fidelity from the bench reads as a closed courthouse door from the litigant's position. Living-constitutionalist judges hold institutional power but trapped exit — bound by a methodology they reject, able only to dissent — so their seat computes suppression without extraction relief. Professional historians sit near-symmetric: the arrangement increased demand for their expertise while subjecting their craft to judicial misuse. The engine computes this divergence from power, exit, and role data; this story authors the structural facts, not the verdicts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structural relationships: state_governments and elected_legislatures collect the arrangement's doctrinal wins (federalism limits, deference, regulatory freedom from unenumerated-rights challenges) — low d, damped effective extraction. originalist_legal_movement collects the reproduction mechanism — appointments, clerkships, gatekeeping — low d with a professional-identity component. unenumerated_rights_claimants bear the burden directly (foreclosed claims, asymmetric litigation costs) with trapped exit — high d, amplified. historically_disenfranchised_groups bear a second-order burden: the rule demands a protective tradition that their own founding-era exclusion prevented from forming — trapped, high d. living_constitutionalist_judges pay in foreclosed methodology; state_supreme_courts_rejecting_originalism are excluded rather than coordinated. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the correct d for every seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the countermajoritarian difficulty — is live: no reading has dissolved the legitimacy question, and the parties dispute which reading solves it, so the arrangement has not outlived its function and mandatrophy is not declared. The tangled_rope classification prevents two mislabels. Reading the arrangement as pure extraction (snare) would erase the genuine coordination function: judicial discretion really is a collective-action problem, and the arrangement really does bind governments as well as litigants. Reading it as pure coordination (rope) would erase the asymmetric burden: the historical gate falls hardest on those least able to fund it and on groups whose exclusion shaped the very record they must cite. Identity-lock dynamics govern persistence: the majority's commitment is fused at the professional level (jurisprudential legacy), the ideological level (movement membership), and the institutional level (the Court's current composition); if that identity frame broke through personnel turnover or a landmark re-theorization, enforcement would decay quickly, because the arrangement's suppressive force depends on who holds the bench rather than on self-sustaining structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the constitutional_text_authority kernel (originalist_reading). Is the kernel''s authority structure best instantiated by fixed ratification-era meaning (this reading), evolving contemporary meaning (living_constitutionalist_reading), or enactment pedigree alone (positivist_reading)?',
    'No empirical test settles it — the dispute turns on prior commitments about the source of law''s authority. Comparative institutional analysis and legitimacy-outcome studies can inform but not resolve the choice of reading.',
    'Adopting the living reading instantiates a different constraint with a different victim set (courts'' discretion constrained differently, different claims succeed); this story''s ε would not transfer across readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: which reading of the constitutional-text-authority kernel this constraint instantiates and what siblings would change.').

omega_variable(
    historical_referent_ambiguity,
    'Whose understanding fixes constitutional meaning — the drafters'' subjective intent, the ratifiers'' understanding, general public meaning, or original methods? The reading''s own tradition has shifted among these referents.',
    'Conceptual analysis plus systematic archival study of which referent judicial opinions actually deploy when they claim historical grounding.',
    'Each referent yields a different determinacy profile and therefore a different suppression and extraction signature; per the ε-invariance principle, distinct referents may be distinct constraints rather than one constraint with a measurement parameter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_referent_ambiguity, conceptual, 'Within-reading ambiguity about the historical referent the fixed-meaning rule actually binds judges to.').

omega_variable(
    law_office_history_share,
    'What share of judicial historical reasoning is outcome-driven selection rather than good-faith application of the methodology?',
    'Systematic coding of opinions against professional historiography standards — pilot studies of this kind already exist and could be scaled.',
    'A high law-office share would raise theater_ratio, push the arrangement toward degraded or extractive drift, and undermine its coordination claim — the fidelity story would be mostly performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_office_history_share, empirical, 'Empirical share of performative versus functional historical reasoning in originalist adjudication.').

omega_variable(
    article_v_exit_reality,
    'Does the Article V amendment threshold constitute a genuine exit channel for those harmed by fixed meaning, or is it a de facto lock given the difficulty of assembling the required supermajorities on contested social questions?',
    'Amendment-frequency analysis and comparative study of formal versus effective amendment thresholds across constitutional systems.',
    'If Article V is a de facto lock, the arrangement''s effective suppression exceeds its formal exit structure: trapped payers have no real remedy channel, and the democratic-legitimacy justification weakens correspondingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_v_exit_reality, empirical, 'Whether the formal amendment channel functions as a real exit or a structural dead end.').

omega_variable(
    judicial_identity_lock_persistence,
    'Is the originalist majority''s commitment identity-fused (professional, ideological, and institutional identity fused with the methodology) or conviction that would survive personnel change?',
    'Track doctrinal stability across future appointments and retirements; observe whether any justice revises methodology after the social pressure that produced it recedes.',
    'If identity-fused, enforcement decays only with personnel turnover and the arrangement''s persistence is contingent on bench composition; if conviction-based, the arrangement survives personnel change and suppression persists independent of its current administrators.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_identity_lock_persistence, empirical, 'Identity-lock basis of the enforcement coalition and its consequences for persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__originalist_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cons_tr_t9, constitutional_text_authority__originalist_reading, theater_ratio, 9, 0.3).
narrative_ontology:measurement(cons_tr_t18, constitutional_text_authority__originalist_reading, theater_ratio, 18, 0.34).
narrative_ontology:measurement(cons_tr_t27, constitutional_text_authority__originalist_reading, theater_ratio, 27, 0.38).
narrative_ontology:measurement(cons_tr_t36, constitutional_text_authority__originalist_reading, theater_ratio, 36, 0.41).
narrative_ontology:measurement(cons_tr_t45, constitutional_text_authority__originalist_reading, theater_ratio, 45, 0.42).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__originalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cons_be_t9, constitutional_text_authority__originalist_reading, base_extractiveness, 9, 0.42).
narrative_ontology:measurement(cons_be_t18, constitutional_text_authority__originalist_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(cons_be_t27, constitutional_text_authority__originalist_reading, base_extractiveness, 27, 0.52).
narrative_ontology:measurement(cons_be_t36, constitutional_text_authority__originalist_reading, base_extractiveness, 36, 0.56).
narrative_ontology:measurement(cons_be_t45, constitutional_text_authority__originalist_reading, base_extractiveness, 45, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__originalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t9, constitutional_text_authority__originalist_reading, suppression_requirement, 9, 0.38).
narrative_ontology:measurement(cons_su_t18, constitutional_text_authority__originalist_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(cons_su_t27, constitutional_text_authority__originalist_reading, suppression_requirement, 27, 0.54).
narrative_ontology:measurement(cons_su_t36, constitutional_text_authority__originalist_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement(cons_su_t45, constitutional_text_authority__originalist_reading, suppression_requirement, 45, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__originalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional interpretation' conflates three structurally distinct authority claims about the same kernel text: fixed ratification-era meaning (this file), evolving contemporary meaning (living_constitutionalist_reading), and enactment-pedigree validity without moral content (positivist_reading). Each is a separate constraint with its own ε, beneficiary/victim structure, and classification. This reading's ε referent is the fixed-meaning arrangement as it operates, not the arrangements its rivals would install. The family is linked via affects_constraints: the originalist reading forecloses the living reading within any single framework of constitutional authority and coexists with the positivist reading, whose validity theory is compatible with originalist interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
