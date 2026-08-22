% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Positivist Reading of U.S. Constitutional Meaning (Text Plus Amendments)
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the us_constitution_1787 kernel:
 *   the positivist reading, under which constitutional meaning consists of
 *   the enacted text plus formally adopted amendments and nothing else, with
 *   judicial interpretation confined to that material. Per the
 *   epsilon-invariance principle, the sibling readings (originalist_reading,
 *   living_reading) are separate constraints with their own epsilon values,
 *   beneficiary/victim structures, and classifications; they are linked
 *   through network.affects_constraints and are not described or averaged
 *   inside this file. The standing arrangement under contest — and therefore
 *   the referent of the authored extractiveness — is the text-bound
 *   interpretive regime itself, assessed by this reading's own lights: a
 *   mostly legitimate coordination settlement whose costs fall visibly and
 *   increasingly on those the text does not mention. KEY AGENTS (by
 *   structural relationship): supreme_court_justices — administering
 *   agenda-setter who is also bound by the rule
 *   (institutional/identity_locked); federal_and_state_legislatures — primary
 *   beneficiary (institutional/mobile); article_v_amendment_mobilizers —
 *   beneficiary holding the exclusive change channel (organized/mobile);
 *   ordinary_citizens — diffuse beneficiary with incidental exposure
 *   (organized/constrained); unenumerated_rights_claimants — primary target
 *   (powerless/trapped); discrete_minority_groups — secondary target
 *   (powerless/trapped); senate_confirmation_gatekeepers — enforcement-side
 *   agenda-setter (institutional/constrained);
 *   living_constitutionalism_advocates — excluded rival methodology
 *   (powerful/trapped); constitutional_theorists — analytical observer
 *   (moderate/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.44).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.56).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.56).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Positivist Reading of U.S. Constitutional Meaning (Text Plus Amendments)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'fea2359b-5718-4ad3-89aa-950c84364c06').
narrative_ontology:cs_kernel_codification('fea2359b-5718-4ad3-89aa-950c84364c06', fixed_text).
narrative_ontology:cs_authority_grounding('fea2359b-5718-4ad3-89aa-950c84364c06', lineage).
narrative_ontology:cs_interpretation_layer_present('fea2359b-5718-4ad3-89aa-950c84364c06').
narrative_ontology:cs_reading_relation('fea2359b-5718-4ad3-89aa-950c84364c06', us_constitution_1787__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('fea2359b-5718-4ad3-89aa-950c84364c06', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_axiom('fea2359b-5718-4ad3-89aa-950c84364c06', foundational, enacted_text_exhaustive_authority).
narrative_ontology:cs_axiom_status(enacted_text_exhaustive_authority, holdable).
narrative_ontology:cs_axiom_grounding('fea2359b-5718-4ad3-89aa-950c84364c06', enacted_text_exhaustive_authority, conventional).
narrative_ontology:cs_axiom('fea2359b-5718-4ad3-89aa-950c84364c06', foundational, article_v_exclusive_update_mechanism).
narrative_ontology:cs_axiom_status(article_v_exclusive_update_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('fea2359b-5718-4ad3-89aa-950c84364c06', article_v_exclusive_update_mechanism, conventional).
narrative_ontology:cs_reference_frame('fea2359b-5718-4ad3-89aa-950c84364c06', enacted_text_plus_formal_amendment).
narrative_ontology:cs_drift_state('fea2359b-5718-4ad3-89aa-950c84364c06', contemporary_doctrine, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fea2359b-5718-4ad3-89aa-950c84364c06', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, federal_and_state_legislatures).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, article_v_amendment_mobilizers).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, ordinary_citizens).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, discrete_minority_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, senate_confirmation_gatekeepers).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, supreme_court_justices).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, ordinary_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide cases under a rule that confines constitutional argument to the enacted text as amended. The rule is one they administer and are simultaneously subject to: applying it means forgoing recourse to moral reasoning, social evolution, or historical intent when deciding hard cases. Life tenure means the method a justice brings to the bench tends to persist for decades and fuses with that justice's jurisprudential self-conception; retirement is the only exit.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, supreme_court_justices, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, supreme_court_justices, payer).

% Enact statutes knowing that courts reviewing them may consult only the constitutional text. Laws touching interests the text does not mention face little risk of invalidation, so the space of legislatable policy widens. They lose nothing under the arrangement and can operate anywhere within it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, federal_and_state_legislatures, beneficiary,
    institutional, generational, mobile, national).

% Assemble the supermajority coalitions the arrangement recognizes as the only way constitutional meaning changes. Building such a coalition requires sustained organization across two-thirds of Congress and three-quarters of the states over many years, so the channel rewards patient, well-funded movements and is rarely used.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, article_v_amendment_mobilizers, beneficiary,
    organized, generational, mobile, national).

% Live under rules they can look up and expect courts to apply consistently, which is worth a great deal in daily planning. The same arrangement leaves interests the text never mentions — aspects of family life, bodily autonomy, emerging technology — without federal constitutional protection unless an amendment passes.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, ordinary_citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, ordinary_citizens, payer).

% Go to court because ordinary politics did not protect them, and find that the interpretive rule counts only what the text says. The prescribed remedy — constitutional amendment — costs more than any individual litigant or small group can mount. Staying in court means losing on the merits; leaving means abandoning the claim.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Depend on enforceable limits on what majorities may do to them. Where the text addresses them directly, the rule serves them well; where it is silent, their protection reduces to majority self-restraint exercised through the very legislatures the rule shields from judicial oversight.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, discrete_minority_groups, payer,
    powerless, generational, trapped, national).

% Screen judicial nominees for the interpretive method they will carry onto the bench, turning method selection into appointment politics. They gain durable leverage over the courts' future composition; their own positions depend on winning elections, which keeps their horizons shorter than the justices they confirm.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, senate_confirmation_gatekeepers, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, senate_confirmation_gatekeepers, beneficiary).

% Hold that constitutional meaning develops with society and argue the point in scholarship, opinions, and classrooms. Inside this reading's framework their method is defined as illegitimate rather than answered, so their objections register only from outside; entering the framework would require renouncing their core premise.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalism_advocates, excluded,
    powerful, generational, trapped, national).

% Analyze the arrangement from no seat inside it — comparing readings, tracing downstream consequences, and documenting where the text-only rule decides cases cleanly and where it goes quiet. They collect nothing and pay nothing under it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_theorists, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, federal_and_state_legislatures).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives courts a determinate decision procedure — apply the enacted text — and preserves popular control over fundamental law by reserving all constitutional change to the amendment process. This addresses the counter-majoritarian objection to judicial review and the indeterminacy of open-ended interpretation.
% TRANSFER_FUNCTION: Moves the authority to say what the Constitution requires away from judges' moral and historical judgment and toward the enacted text. Concretely, it moves the power to create new constitutional entitlements from litigants-and-courts to supermajority amendment coalitions, and moves statutory policy space from judicial discretion to legislatures.
% ABSENT_VOICES: Unenumerated-rights claimants would object that the framework decides their fate by definitional fiat, and living-constitutionalist jurists and scholars would object that the framework treats their method as illegitimate rather than refuting it. Neither sits inside the framework's conversation: the first because their claims are non-textual by hypothesis, the second because admission requires renouncing their premise.
% DISAPPEARANCE_RATIONALE: Courts would resume drawing on sources outside the text — societal change, moral argument, historical intent — unenumerated-rights dockets would reopen, legislatures would face unpredictable invalidation of statutes touching unmentioned interests, and the amendment process would lose its position as the only route by which fundamental law changes.
% FOUNDING_PROBLEM: Judicial review by unelected judges overriding democratically enacted law lacked a principled limit: without a fixed source of authority, constitutional adjudication threatened to become judges' policy preferences enforced against the majority.
% FOUNDING_PROBLEM_CORROBORATION: Cross-methodological scholarship attests the underlying problem: originalist and living-constitutionalist writers who reject the positivist solution nonetheless document the counter-majoritarian difficulty and the indeterminacy of unbounded interpretation. The recurring intensity of Supreme Court confirmation fights, recorded in Senate Judiciary Committee proceedings, shows the legitimacy question remains unsettled well outside the textualist camp.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.44 (moderate): the text-only rule delivers real coordination value, but its burdens concentrate on claimants whose interests lack textual anchors, and those burdens grow as the promised alternative — amendment — becomes practically unreachable. Suppression is 0.56 and is authored as a RAW structural property: it reflects the confirmation-screening, precedent-hierarchy, and professional-norm machinery that holds judges to text, and unlike extractiveness it receives no scaling from power or scope in the engine's computation. Theater ratio 0.31: the method's functional content (determinacy, legitimacy) is real, but a growing share of 'we apply the text' output involves construction sophisticated enough to be difficult to distinguish from ordinary judgment. Accessibility collapse 0.48: within the framework, non-textual interpretation collapses almost completely once the premise is granted, but the framework itself competes openly with two sibling frameworks, so meta-level alternatives persist. Resistance 0.62: sustained opposition from a large segment of the bench, bar, and academy. The temporal series run on ONE shared grid (t=0,10,20,30,40,50,60, roughly 1965-2025) with all three metrics authored at every point. Three trajectories tell one story: base_extractiveness rises (0.28 to 0.44) as the amendment channel decays into practical disuse, making the 'your remedy is amendment' transfer uncompensated for more claimants; theater_ratio rises (0.14 to 0.31) as plain-meaning rhetoric spreads while construction complexity grows; suppression_requirement rises (0.38 to 0.56) because holding the bench to text now requires deliberate, ideologically screened appointments against entrenched contrary practice — the enforcement infrastructure had to be built up over the interval, which is why suppression_requirement is tracked rather than left static. Fixing the settlement — replacing the interpretive regime — is prohibitive for any single actor: it requires either Article V supermajorities or a multi-decade project of methodologically aligned appointments.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat, the text-only rule is a self-imposed discipline that purchases legitimacy and determinacy — the justices experience it as the price of their office's authority. From the legislature seat, it is protected policy space. From the unenumerated-claimant seat, the identical rule operates as closure: a door shut by definitional fiat, with the exit it points to (amendment) priced beyond reach. The identity-lock mechanism on the justices is professional and institutional: a justice's interpretive method is fused with their jurisprudential self-conception and confirmed publicly at nomination, so breaking the frame would require repudiating their own established identity, not merely changing a policy preference. The two victim classes have coalition potential only in principle — the dispersion and resource poverty that put them in court in the first place are the same facts that prevent them from assembling an Article V coalition, which is why their exit is modeled as trapped rather than constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: legislatures (institutional, mobile) sit near the subsidized end — the rule widens their space at no cost to them; amendment mobilizers hold the exclusive channel the rule creates; ordinary citizens collect predictability while carrying diffuse indirect exposure (modeled via secondary_role payer). Victims derive high directionality: unenumerated claimants and discrete minority groups (both powerless, both trapped) sit near the full-target end — the rule takes from them precisely the judicial forum they had turned to, and their trapped exit amplifies effective extraction. The justices occupy a genuinely dual position: they administer the rule and are disciplined by it, which places them near symmetric rather than at either pole. No directionality_overrides are authored: the derivation chain distinguishes the same-power-atom seats adequately through exit-option variation (institutional-and-mobile legislatures versus institutional-and-identity_locked justices), and an override keyed to the institutional power atom would wrongly move the legislatures along with the justices. Gain flow: the gains of the arrangement demonstrably accrue to the legislative seats — the widened, judicially unreviewable policy space lands there — so gain_flow names federal_and_state_legislatures rather than asserting diffusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating judicial review against the counter-majoritarian objection — is live, not dead: every confirmation cycle re-litigates it, and the corroboration comes from outside the beneficiary set (rival-methodology scholars who reject this solution while documenting the problem it targets). Because the founding problem is live and the world rearranges if the arrangement vanishes, this is not a piton and mandatrophy is not resolved; no sunset clause applies because the arrangement claims permanence, not transition. The tangled_rope classification does specific work here: declaring the coordination function (counter-majoritarian channeling, determinacy) prevents misreading the settlement as pure taking — a snare verdict would erase the genuine legitimacy service the rule performs for majorities and citizens alike; declaring the victims (unenumerated claimants, discrete minorities) and the active enforcement requirement prevents misreading it as pure coordination — a rope verdict would erase the fact that the same structure that channels democratic change also prices unenumerated interests out of constitutional protection. The rising extraction trajectory is the number to watch: if the amendment channel continues to decay, the coordination half shrinks relative to the taking half, and the computed type should migrate toward the snare boundary — that migration, not any static label, is the finding this story is built to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the us_constitution_1787 kernel; would the originalist or living readings produce a structurally different beneficiary/victim set and a different computed type?',
    'Generate the sibling readings as separate stories and compare computed types, victim sets, and extraction profiles across the kernel family.',
    'If the living reading computes with materially lower burden on unenumerated claimants, this reading''s cost asymmetry is reading-relative rather than topic-intrinsic; if all three compute similar asymmetries, the asymmetry belongs to the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three rival exhaustive-source readings of a single constitutional kernel.').

omega_variable(
    amendment_channel_viability,
    'Is the Article V amendment process a realistically available remedy for groups the text does not protect, or is it so costly as to be practically closed?',
    'Compare the historical base rate of rights-expanding amendments and documented failed campaigns against the volume of unenumerated-rights litigation over the interval.',
    'If the channel is effectively closed, the ''remedy is amendment'' transfer imposes uncompensated burdens and the arrangement reads as far more taking than coordinating, pushing the computed type toward the snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_channel_viability, empirical, 'Whether the prescribed exit for unenumerated claimants actually exists in practice.').

omega_variable(
    plain_meaning_determinacy,
    'Does text-bound interpretation actually deliver determinate answers, or does construction under ''plain meaning'' reintroduce judge-level discretion?',
    'Inter-circuit divergence rates and Supreme Court reversal analysis restricted to holdings that announce themselves as applications of plain textual meaning.',
    'High residual discretion would raise the theater share of the method, weaken the coordination claim, and erode the determinacy benefit that anchors the citizen-seat benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plain_meaning_determinacy, empirical, 'Whether the method''s central advertised product — determinacy — survives contact with hard cases.').

omega_variable(
    minority_protection_substitutability,
    'Do state constitutions, federal statutes, and political mobilization adequately substitute for judicially recognized unenumerated federal rights?',
    'Comparative outcome tracking for the same interests under textual versus non-textual regimes across state systems and statutory schemes.',
    'Adequate substitutes lower the real burden on unenumerated claimants and soften the extraction asymmetry; inadequate substitutes deepen it and strengthen the victim declarations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_substitutability, empirical, 'Whether the burden borne by the target seats is mitigated by substitute protection elsewhere.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__positivist_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__positivist_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__positivist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_1787__positivist_reading, theater_ratio, 60, 0.31).
narrative_ontology:measurement_basis(us_c_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__positivist_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__positivist_reading, base_extractiveness, 20, 0.34).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__positivist_reading, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement_basis(us_c_be_t50, observed).
narrative_ontology:measurement(us_c_be_t60, us_constitution_1787__positivist_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(us_c_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_1787__positivist_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t10, us_constitution_1787__positivist_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(us_c_su_t10, observed).
narrative_ontology:measurement(us_c_su_t20, us_constitution_1787__positivist_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(us_c_su_t20, observed).
narrative_ontology:measurement(us_c_su_t30, us_constitution_1787__positivist_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement_basis(us_c_su_t30, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_1787__positivist_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_1787__positivist_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement_basis(us_c_su_t50, observed).
narrative_ontology:measurement(us_c_su_t60, us_constitution_1787__positivist_reading, suppression_requirement, 60, 0.56).
narrative_ontology:measurement_basis(us_c_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'how the Constitution means' covers three structurally distinct claims, written here as three stories sharing the us_constitution_1787 kernel. This (positivist) story authors epsilon for the text-plus-amendments regime as the positivist sees it; the originalist sibling authors epsilon for a ratification-fixed regime; the living sibling authors epsilon for an evolving-doctrine regime. Each member links to the others via network.affects_constraints; the originalist reading functions partially upstream of this one (text-centrality arguments are cited in support of both), while the living reading contests both from below.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
