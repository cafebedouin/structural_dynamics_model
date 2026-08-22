% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Ending (End-of-Life Authority Kernel, Sanctity Reading)
 *   domain: bioethics/end-of-life policy
 *
 * SUMMARY:
 *   This story instantiates the sanctity_reading of the contested kernel
 *   end_of_life_decision_authority. The standing arrangement under contest is
 *   the civil and professional prohibition on intentional life-ending,
 *   grounded in the claim that human life's value does not depend on its
 *   bearer's will and therefore cannot be waived by it. The regime operates
 *   through criminal codes, licensing discipline, and doctrinal teaching, and
 *   it is defended against a sustained autonomy movement and a growing set of
 *   permissive jurisdictions. KEY AGENTS (by structural relationship): -
 *   competent_suffering_patients_denied_exit: primary target
 *   (powerless/trapped) — bears the imposed continuation of unwanted dying -
 *   pressured_vulnerable_dependents: principal protected beneficiary
 *   (powerless/constrained) - medical_profession: beneficiary and
 *   co-administrator (institutional/constrained) -
 *   religious_traditional_communities: doctrinal beneficiary
 *   (organized/constrained) - disability_rights_community: organized
 *   beneficiary defending the line - bereaved_and_caregiving_families:
 *   dual-positioned — bear prolonged costs, receive complicity-sparing -
 *   legislators_and_constitutional_courts: agenda setter
 *   (institutional/mobile) — the only seat able to move the line -
 *   right_to_die_advocacy_movements: organized opposition bearing continuous
 *   political cost - comparative_bioethics_scholars: analytical observer
 *   Structural delta versus the sibling readings: under this reading the
 *   pressured-vulnerable sit in the beneficiary set (the bright line shields
 *   them from burden-driven pressure), the physician's role is confined to
 *   healing, and the costs of continued unwanted dying are externalized —
 *   borne privately by sufferers and their families with no institutional
 *   response. Per the claim/metric independence rule, claimed_type records
 *   the structural analysis (a genuine coordination function plus an
 *   asymmetrically borne burden plus active enforcement), while
 *   extractiveness is authored reading-indexed per OQ-26: the referent is the
 *   prohibition regime itself, assessed by the sanctity reading's own lights,
 *   which deny that any waivable entitlement is being taken. The engine
 *   computes per-seat classifications from the structural data; divergence
 *   between seats is the measurement this story exists to take.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.14).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.62).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity-of-Life Prohibition on Intentional Life-Ending (End-of-Life Authority Kernel, Sanctity Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, 'db300288-b5c9-4012-8aea-b7d9d5bb8388').
narrative_ontology:cs_kernel_codification('db300288-b5c9-4012-8aea-b7d9d5bb8388', formalized).
narrative_ontology:cs_authority_grounding('db300288-b5c9-4012-8aea-b7d9d5bb8388', lineage).
narrative_ontology:cs_interpretation_layer_present('db300288-b5c9-4012-8aea-b7d9d5bb8388').
narrative_ontology:cs_reading_relation('db300288-b5c9-4012-8aea-b7d9d5bb8388', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('db300288-b5c9-4012-8aea-b7d9d5bb8388', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('db300288-b5c9-4012-8aea-b7d9d5bb8388', foundational, life_value_independent_of_will).
narrative_ontology:cs_axiom_status(life_value_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('db300288-b5c9-4012-8aea-b7d9d5bb8388', life_value_independent_of_will, deontological).
narrative_ontology:cs_axiom('db300288-b5c9-4012-8aea-b7d9d5bb8388', secondary, healer_role_excludes_intentional_killing).
narrative_ontology:cs_axiom_status(healer_role_excludes_intentional_killing, holdable).
narrative_ontology:cs_axiom_grounding('db300288-b5c9-4012-8aea-b7d9d5bb8388', healer_role_excludes_intentional_killing, conventional).
narrative_ontology:cs_reference_frame('db300288-b5c9-4012-8aea-b7d9d5bb8388', life_as_inviolable_gift).
narrative_ontology:cs_drift_state('db300288-b5c9-4012-8aea-b7d9d5bb8388', contemporary_post_legalization_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('db300288-b5c9-4012-8aea-b7d9d5bb8388', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_dependents).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, medical_profession).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, disability_rights_community).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_traditional_communities).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, competent_suffering_patients_denied_exit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, bereaved_and_caregiving_families).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, bereaved_and_caregiving_families).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, right_to_die_advocacy_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with terminal illness or irreversible suffering who have decided they want their lives to end sooner than disease will allow and ask a physician to help. The law answers no. Their lawful options narrow to continuing to live in the unwanted condition, declining further treatment (which many conditions render unavailable or merely slower), or paying to travel to a permissive country while still healthy enough to qualify there. Money, prognosis speed, and physical capacity determine which of these remain open.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, competent_suffering_patients_denied_exit, payer,
    powerless, immediate, trapped, national).

% Elderly and disabled people who depend on others for daily care and whose care is costly to families and systems. Because intentional life-ending is unlawful in every circumstance, no one — family member, caregiver, insurer, or physician — can lawfully bring their death closer, however heavy the burden of their care becomes. The bright line gives them a fixed social answer when they worry about what their dependence makes others wish.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_dependents, beneficiary,
    powerless, biographical, constrained, national).

% Physicians and the bodies that license them. Professional ethics codes and criminal law both define the physician's role as healing, never killing; licensing bodies write the codes, investigate violations, and revoke licenses. The profession carries the duty of refusing requests it cannot grant and the assurance that its members cannot be recruited as instruments of anyone's death.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_profession, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, medical_profession, agenda_setter).

% Organizations of disabled people who argue that offering death as a remedy communicates that their lives are worth less and that eligibility criteria inevitably expand. Prohibition holds the line they defend; they lobby legislatures, file amicus briefs, and campaign wherever legalization bills appear.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, disability_rights_community, beneficiary,
    organized, generational, constrained, national).

% Communities whose doctrines hold life as a gift held in trust rather than property of the bearer. Civil prohibition aligns public law with their teaching; they preach, publish, and mobilize politically to keep it aligned, and they operate hospice and palliative services as the practical expression of the same commitment.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_traditional_communities, beneficiary,
    organized, generational, constrained, global).

% Families who watch a member live on in suffering they cannot lawfully shorten, carrying years of caregiving labor, cost, and anticipatory grief. The same bright line spares them ever having to decide whether to participate in a death, or wondering afterward whether an inheritance motive colored a relative's request.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bereaved_and_caregiving_families, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, bereaved_and_caregiving_families, beneficiary).

% Parliaments and high courts that write, uphold, amend, or strike the prohibition. They hear the recurring challenge cycle — autonomy claims, equality claims, slippery-slope evidence — and several have moved the line while most have kept it. They are the only seat that can change the arrangement outright.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, legislators_and_constitutional_courts, agenda_setter,
    institutional, generational, mobile, national).

% Membership organizations campaigning for legal access to assisted dying. Their members include people currently denied the option; the organizations spend continuously on litigation, ballot initiatives, and legislative testimony, absorbing losses in most venues while winning in a growing few.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, right_to_die_advocacy_movements, payer,
    organized, biographical, constrained, national).

% Academic ethicists, clinicians, and legal scholars who map the arguments across jurisdictions and traditions, document outcomes where the line has moved, and supply the evidentiary record that both sides cite.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, comparative_bioethics_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: a single bright line against intentional killing, administered uniformly, removes case-by-case end-of-life judgment from families, insurers, and physicians — giving dependent people a socially unambiguous guarantee that their care burden cannot lawfully be resolved by their death, keeping the healer role unmixed with killing, and sparing every party repeated discretionary decisions made under grief and fatigue.
% TRANSFER_FUNCTION: Withdraws the option of assisted death from those who would choose it and imposes continued living on them; distributes in exchange a guarantee of non-killing to dependents, role stability to physicians, doctrinal alignment to religious communities, and complicity-sparing to families. What moves is decision authority over the timing of death: from the individual bearer to the collective moral-legal order.
% ABSENT_VOICES: Competent sufferers themselves are largely absent from the forums where the line is defended: they are medically incapacitated, isolated, or represented only through proxy organizations; disabled people who want the option are weaker in these venues than the organized disability opposition; and future patients have no seat at all. Part of the beneficiary coalition's unanimity is an artifact of who could physically be in the room.
% DISAPPEARANCE_RATIONALE: If the prohibition vanished overnight, assisted dying would emerge quickly wherever demand and medical capacity exist; burden-driven pressure on dependent elders and disabled people would activate immediately, since the current guarantee is the line itself; the physician's role would renegotiate around a new permission to kill; and religious communities would lose the public-law alignment of their teaching. Nothing about the underlying diseases, dependencies, or family economics stays put — the arrangements of care, trust, and inheritance all re-price.
% FOUNDING_PROBLEM: The arrangement descends from the oldest prohibitions on private killing, consolidated in the modern era against two remembered horrors: the medical killing programs of the eugenics period, and the fear that any lawful door opened for mercy becomes a corridor of pressure on the dependent. It was built to keep killing out of medicine and out of the reach of anyone's convenience — including the sufferer's own exhausted request.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: every jurisdiction that has legalized assisted dying rebuilt its statute around the founding risks (eligibility limits, waiting periods, psychiatric review, reporting regimes) — an implicit admission that the risks are real; the twentieth-century historical record of medical killing programs is uncontested; and disability-rights scholarship independently documents devaluation dynamics. No major participant in the debate, on either side, asserts the founding problems are dead.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.14, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).
:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.14 as a reading-indexed value: assessed by the sanctity reading's own lights, the prohibition takes nothing anyone rightfully holds, because the reading denies that a competence to be killed exists to be overridden; the small residual registers the reading's own hard-case literature, which concedes that sufferers bear a real and uncompensated burden for the sake of the absolute. Suppression (0.62) is authored as a raw structural property — criminal statutes, professional discipline, prosecution of helpers, resistance to extraterritorial evasion — and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater ratio (0.22) reflects a regime whose core function still operates but whose margin increasingly performs: reaffirmation rituals, symbolic prosecutions, and doctrinal assertion persist while practice accommodates through sedation and withdrawal. Accessibility collapse (0.38): alternatives do not fully close — treatment refusal remains lawful everywhere, palliative sedation is widely available, and jurisdictional travel stays open to those with money and time. Resistance (0.58): court challenges, ballot campaigns, civil disobedience by physicians, and an international advocacy movement meet the line continuously. The temporal series share one grid (t=0..50, mapping roughly to 1975–2025, from the Quinlan era through two decades of legalized assisted dying elsewhere): reading-indexed extractiveness creeps up as hard cases accumulate; theater rises as neighboring jurisdictions legalize and the line's defense grows more declaratory; the suppression requirement declines gently as enforcement shifts from prosecuting patients to regulating helpers. Coalition note: the payer seat is structurally hard to organize — each sufferer's crisis is individual, short, and isolating — so resistance is carried by surrogates (advocacy organizations, sympathetic clinicians) rather than by payers as a class. Coordination type is declared identity_coordination because the dominant function is boundary maintenance — what physicians are, what communities hold inviolable, what dependence entitles no one to do; failure of the norm, not of a resource mechanism, is what activates the pressure cascade. The default floor for that type is used; no override is justified.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the payer seat (competent sufferers), the arrangement is totalizing: public law reaches into the most intimate decision a person faces and answers it in advance, permanently, with no exception for their case. From the protected-dependent seat, the same arrangement is a shield: a fixed social answer ensuring no one can lawfully convert their dependence into their death. From the profession's seat it is constitutive — the healer identity is fused with the refusal to kill, and exit from the norm would mean exit from medicine's self-definition (an institutional identity lock, distinct from the sufferers' physical-legal trap). From the agenda-setter seat it is a manageable bright line that avoids case-by-case judgment. The engine derives these divergences from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Competent suffering patients are declared victims with trapped exit: their directionality sits near the full-target end, so effective extraction is amplified for them even from the low reading-indexed base. Pressured vulnerable dependents, the medical profession, disability-rights organizations, and religious communities are declared beneficiaries: their directionality sits near the subsidy end — the arrangement costs them little and insures them much. Bereaved and caregiving families are dual-positioned (declared victims, holding a secondary beneficiary position): mid-range directionality. Legislatures and courts are the agenda-setting seat; their directionality comes from the canonical fallback, and their defining feature is that they alone can move the line at will. No directionality overrides are authored: the beneficiary/victim structure plus exit options already separates the seats, and the coarse power-atom override surface would add noise rather than precision.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problems — preventing abuse of killing power, keeping healers from becoming killers, protecting lives that others' utility calculations would discount — remain live: every permissive jurisdiction rebuilds safeguards against exactly these risks, which is corroboration from outside the beneficiary set. The mandate has not outlived its function, so no mandatrophy resolution is declared. The tangled_rope claim is what prevents mislabeling in both directions: reading the structure without its coordination half yields a pure-extraction verdict against sufferers; reading it without its extraction half yields a pure-coordination or natural-law verdict that erases the imposed burden. Both halves are present in the data. The receipt surface records that no seat captures what is taken — the sufferers' foregone exit is destroyed, not transferred — while fixing remains institutionally cheap wherever a legislature's coalition shifts, which is why the arrangement persists by conviction and enforcement rather than by capture or inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_death_authority,
    'This story instantiates the sanctity_reading of kernel end_of_life_decision_authority; the autonomy_reading and the vulnerability_protection_reading instantiate different constraints from the same kernel. Which reading governs a given jurisdiction, and what changes structurally when the governing reading switches?',
    'Track adoption and adjudication: statutory legalization, constitutional rulings, and professional-body reversals mark reading switches; compare victim-set boundaries and safeguard architectures across regimes.',
    'Under the autonomy_reading the pressured-vulnerable move into the victim set and the competent sufferer leaves it; under the vulnerability_protection_reading both seats are partly protected and partly exposed through checkpoint design; the physician''s role scope and the location of suffering costs move with each switch.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_death_authority, conceptual, 'Committer structure: this constraint is one of three readings of the end-of-life-authority kernel.').

omega_variable(
    intrinsic_value_natural_vs_constructed,
    'Is the intrinsic value of human life a mind-independent fact the prohibition merely records, or a constructed norm whose persistence serves identifiable beneficiaries (protected dependents, the profession''s identity, doctrinal communities)?',
    'Metaethical analysis plus beneficiary-tracking: test whether the norm''s application tracks beneficiary interests across cases (war, self-defense, capital punishment, treatment withdrawal are commonly excepted) or applies uniformly as a genuine absolute would.',
    'If constructed-with-beneficiaries, the reading''s natural-law self-presentation is false-summit-shaped and the constraint classifies as enforced coordination-plus-extraction; if genuinely absolute, much of the measured enforcement is the price of the moral order itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intrinsic_value_natural_vs_constructed, conceptual, 'Whether the reading''s mountain-like self-presentation survives scrutiny.').

omega_variable(
    coercion_counterfactual_under_legalization,
    'How much coercion of dependent ill people would actually materialize if intentional life-ending were lawful — the counterfactual on which the prohibition''s main protective benefit rests?',
    'Natural experiments: Oregon, Benelux, and Canadian assisted-dying data on reported motives (burden-feeling, loneliness), safeguard breaches, and eligibility expansion trajectories, compared against prohibition-era baselines.',
    'If pressure materializes broadly, the protective coordination function is weighty and the tangled-rope structure firms; if it stays marginal, the prohibition''s benefit shrinks toward doctrinal affirmation and the payer-side burden dominates the computed classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_counterfactual_under_legalization, empirical, 'The empirical hinge of the beneficiary claim.').

omega_variable(
    sufferer_testimony_discount,
    'Does the sanctity reading''s own accounting systematically discount the testimony of those who beg for exit — that is, is the reading-indexed extractiveness of 0.14 faithful to the reading, or does the reading undercount imposed suffering by construction?',
    'Internal critique: examine the reading''s hard-case literature (locked-in syndrome, refractory agony) for whether it prices the sufferer''s burden at all, and whether dissenting voices within sanctity traditions (proportionalist theologians, pastoral clinicians) are heard or managed.',
    'If the discount is systematic, the payer seat''s computed burden understates the lived imposition and the corpus should expect harsher per-seat verdicts for the payer seat despite the low authored epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufferer_testimony_discount, preference, 'Whose suffering counts inside the reading''s own lights.').

omega_variable(
    active_passive_boundary_coherence,
    'Is the boundary the reading draws between permitted refusal and withdrawal (and palliative sedation) and forbidden intentional ending principled, or an unstable line that allocates relief by doctrine rather than by the sufferer''s situation?',
    'Doctrine-and-practice audit: track double-effect reasoning, terminal sedation protocols, and withdrawal practice against the categorical premise; observe whether substantively identical intentions receive opposite verdicts across the line.',
    'If the boundary is unstable, part of the enforcement effort defends a distinction rather than a value, raising theater and shifting the classification toward performance-maintained forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_passive_boundary_coherence, conceptual, 'Coherence of the reading''s internal act/omission line.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__sanctity_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__sanctity_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__sanctity_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(end__tr_t50, end_of_life_decision_authority__sanctity_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement(end__be_t50, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 50, 0.14).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(end__su_t50, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The colloquial debate 'euthanasia: yes or no' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints instantiating one kernel: this sanctity_reading (prohibition as intrinsic-value enforcement), end_of_life_decision_authority__autonomy_reading (access as sovereign exercise), and end_of_life_decision_authority__vulnerability_protection_reading (checkpoint-gated access). Each carries its own epsilon, its own beneficiary/victim structure, and its own classification; the victim-set boundary moves between them (the pressured-vulnerable are beneficiaries here and victims under the autonomy reading). Family links are declared via network.affects_constraints in both directions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
