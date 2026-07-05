% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__naturalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__naturalization_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: qwerty_persistence_mechanism__naturalization_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Naturalization / Genuine Adequacy Reading)
 *   domain: economic_history/technology_studies/path_dependence
 *
 * SUMMARY:
 *   In 1873, Christopher Sholes arranged typewriter keys to reduce mechanical
 *   jamming and, per some accounts, to support telegraph operators; the
 *   layout was refined through iterative commercial use and became the de
 *   facto standard by the early 20th century. In the 1930s, August Dvorak
 *   patented an alternative layout claiming substantial speed and ergonomic
 *   gains. Despite over eighty years of availability, free software-level
 *   switching, and periodic promotional pushes, Dvorak and other alternatives
 *   (Colemak, etc.) have captured only a marginal user base. This story
 *   instantiates the NATURALIZATION READING of the QWERTY persistence kernel:
 *   QWERTY persists not because of active suppression or an accident of
 *   frozen coordination, but because it reached and has remained at a
 *   genuinely adequate performance plateau, refined over a century of use,
 *   while alternatives failed to demonstrate a decisive, reproducible
 *   advantage large enough to justify near-universal retraining costs. This
 *   is one of three sibling readings of the same kernel
 *   (qwerty_persistence_mechanism); the lock_in_reading treats the identical
 *   historical facts as evidence of a path-dependent coordination failure
 *   despite technical inferiority, and the beneficiary_extraction_reading
 *   treats them as evidence of active incumbent maintenance protecting
 *   training and market investments. This file does not adjudicate between
 *   them — it presents the naturalization reading on its own structural
 *   terms, with its own epsilon.
 *
 * KEY AGENTS:
 *   - typists_with_existing_skill: Primary incumbent-skill holders (moderate/mobile) — bear real but voluntarily-incurred retraining cost if they switch
 *   - keyboard_manufacturers: Coordination participants (organized/mobile) — produce to standard because it is the standard, not to suppress alternatives
 *   - dvorak_and_alternative_layout_advocates: Excluded/marginal voice (powerless/mobile) — argue for a superior alternative that has not achieved market traction
 *   - new_typists_and_students: Beneficiary of standardization (powerless/mobile) — learn the near-universal layout once and gain broad interoperability
 *   - economic_historians: Analytical observer — examines whether the persistence pattern is best read as adequacy, lock-in, or extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__naturalization_reading, 0.12).
domain_priors:suppression_score(qwerty_persistence_mechanism__naturalization_reading, 0.08).
domain_priors:theater_ratio(qwerty_persistence_mechanism__naturalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__naturalization_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__naturalization_reading, rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__naturalization_reading, "QWERTY Keyboard Layout Persistence (Naturalization / Genuine Adequacy Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__naturalization_reading, "economic_history/technology_studies/path_dependence").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__naturalization_reading, '9b2b3812-6326-4491-86a6-bcd4ed3feeae').
narrative_ontology:cs_kernel_codification('9b2b3812-6326-4491-86a6-bcd4ed3feeae', distributed).
narrative_ontology:cs_authority_grounding('9b2b3812-6326-4491-86a6-bcd4ed3feeae', distributed).
narrative_ontology:cs_reading_relation('9b2b3812-6326-4491-86a6-bcd4ed3feeae', qwerty_persistence_mechanism__lock_in_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b2b3812-6326-4491-86a6-bcd4ed3feeae', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('9b2b3812-6326-4491-86a6-bcd4ed3feeae', foundational, market_outcomes_track_genuine_adequacy).
narrative_ontology:cs_axiom_status(market_outcomes_track_genuine_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('9b2b3812-6326-4491-86a6-bcd4ed3feeae', market_outcomes_track_genuine_adequacy, empirically_contingent).
narrative_ontology:cs_axiom('9b2b3812-6326-4491-86a6-bcd4ed3feeae', foundational, switching_cost_is_private_skill_investment_not_extracted_rent).
narrative_ontology:cs_axiom_status(switching_cost_is_private_skill_investment_not_extracted_rent, holdable).
narrative_ontology:cs_axiom_grounding('9b2b3812-6326-4491-86a6-bcd4ed3feeae', switching_cost_is_private_skill_investment_not_extracted_rent, empirically_contingent).
narrative_ontology:cs_reference_frame('9b2b3812-6326-4491-86a6-bcd4ed3feeae', competitive_standards_equilibrium).
narrative_ontology:cs_drift_state('9b2b3812-6326-4491-86a6-bcd4ed3feeae', contemporary_digital_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9b2b3812-6326-4491-86a6-bcd4ed3feeae', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, typists_with_existing_skill).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, software_and_hardware_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__naturalization_reading, new_typists_and_students).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, market_selection_produces_adequate_standards).
narrative_ontology:constraint_vindicates(qwerty_persistence_mechanism__naturalization_reading, switching_cost_reflects_genuine_investment_not_capture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have invested years building QWERTY muscle memory. They are not prevented from switching to Dvorak or Colemak by any external barrier; free software support exists on every major OS. Their continued use of QWERTY reflects that the accumulated skill remains adequate and the marginal benefit of switching does not clearly exceed the retraining cost, based on their own assessment.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, typists_with_existing_skill, beneficiary,
    moderate, biographical, mobile, global).

% Manufacture physical keyboards printed with QWERTY key labels because that is what the market demands; they also sell Dvorak-labeled and blank/remappable keyboards as a smaller product line. Nothing prevents them from producing alternative layouts at scale if demand materialized; their production choice tracks existing demand rather than actively shaping it.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, keyboard_manufacturers, beneficiary,
    organized, generational, mobile, global).

% Operating systems, input-method frameworks, and hardware all support QWERTY as the default while also offering built-in, free, one-click switching to dozens of alternative layouts including Dvorak and Colemak. The ecosystem's investment in QWERTY-as-default reflects the size of the trained population, not a barrier erected against alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, software_and_hardware_ecosystem, beneficiary,
    institutional, generational, mobile, global).

% Argue that Dvorak and similar layouts offer meaningful speed and ergonomic advantages and have promoted adoption for nearly a century with limited success. They are not blocked from switching or from advocating, but their claims have not translated into mass adoption, which this reading interprets as market feedback about the actual (contested/marginal) magnitude of the advantage rather than evidence of suppression.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, dvorak_and_alternative_layout_advocates, excluded,
    powerless, biographical, mobile, global).

% Learn touch-typing on QWERTY by default in most educational and self-teaching contexts, gaining immediate interoperability with virtually every keyboard, job, and shared device they will encounter. They could choose to learn an alternative layout instead at no financial cost, and a small fraction do.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, new_typists_and_students, beneficiary,
    powerless, biographical, mobile, global).

% Study the QWERTY case as a canonical (and contested) example in path-dependence economics, debating since the 1990s (Liebowitz & Margolis vs. David) whether it demonstrates market failure or market adequacy. Their disagreement is part of what makes this kernel genuinely contested rather than settled.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__naturalization_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A single, near-universal keyboard layout lets any trained typist use almost any keyboard, and lets any manufacturer, educator, or software vendor target one standard rather than fragmenting across competing layouts.
% TRANSFER_FUNCTION: Under this reading, no systematic transfer occurs: retraining costs are borne privately by whoever chooses to switch, and the benefits of standardization (interoperability, shared training infrastructure, manufacturing economies) accrue broadly and diffusely to nearly everyone who uses a keyboard, without a concentrated collector.
% ABSENT_VOICES: Alternative-layout advocates (Dvorak, Colemak communities) have had a public voice for decades via patents, promotional campaigns, and now online communities; they are not silenced, but their claims have not been validated by mass-market uptake. Under this reading their relative quietness in the aggregate outcome reflects unconvincing evidence of net advantage, not suppression — though the omega on Dvorak's contested empirical advantage leaves this genuinely open.
% DISAPPEARANCE_RATIONALE: If QWERTY-as-default vanished overnight (all trained muscle memory and existing keyboards erased), a substantial one-time disruption would occur simply from the scale of retraining required — this argues against world_unchanged. But because this reading holds there is no concentrated beneficiary whose position depends on QWERTY specifically (any similarly-learnable layout would do), the disruption would be a coordination-transition cost, not the collapse of an extraction structure — which argues against world_rearranges in the sense that term carries elsewhere in this corpus (a beneficiary losing an income stream). The naturalization reading itself holds this verdict is properly contested between 'the transition cost is real and large' and 'no one's position structurally depends on this particular layout.'
% FOUNDING_PROBLEM: The original 1870s design problem was preventing frequently-paired typebars on mechanical typewriters from jamming when struck in quick succession, by physically separating common letter-pair keys.
% FOUNDING_PROBLEM_CORROBORATION: Confirmed by typewriter mechanical historians and the general historical record independent of any party with a stake in QWERTY's continuation — no mechanical typewriter with typebars has been in mainstream production for decades, and the jamming problem does not exist on any modern keyboard technology. This is uncontested and attested from entirely outside any beneficiary group named in this story.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__naturalization_reading, contested).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__naturalization_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__naturalization_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(qwerty_persistence_mechanism__naturalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__naturalization_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__naturalization_reading_tests).
:- end_tests(qwerty_persistence_mechanism__naturalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because, under this reading, no party structurally profits from QWERTY's persistence at the expense of an identifiable victim class — manufacturers produce to whatever standard prevails, and existing typists' switching costs are a personal, self-incurred consequence of skill investment, not a rent extracted by anyone. Suppression is authored very low (0.08): nothing prevents an individual, a school, or a company from adopting Dvorak or Colemak today — the layout is a free OS-level setting on every major platform. Theater ratio is low and only mildly rising over the interval (0.05 to 0.10) reflecting a small increase in QWERTY-defense rhetoric (ergonomic-marketing claims by manufacturers) without any corresponding enforcement machinery. Accessibility collapse is moderate-high (0.6): once a typist has invested years in QWERTY muscle memory, switching is costly in a real, non-artificial sense — but this reflects sunk human capital, not blocked alternatives. Resistance is low (0.2): there is no meaningful campaign of resistance against QWERTY because there is no coercive apparatus to resist; Dvorak advocacy exists but is diffuse and small-scale, consistent with a genuine-adequacy equilibrium rather than a contested extraction.
 *
 * PERSPECTIVAL GAP:
 *   From an economic historian's analytical seat, the pattern (century-long dominance, contested alternative, real switching costs) is genuinely ambiguous between naturalization and lock-in explanations — this is exactly the seat divergence the omega variable 'naturalization_vs_lockin_framing' names. From the seat of an individual typist, the lived experience is unambiguous: QWERTY works fine and switching is not worth it, which is evidence consistent with (but not proof of) the naturalization reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, beneficiaries are diffuse and non-concentrated: existing typists benefit from not needing to retrain (a status-quo benefit, not an extracted rent), manufacturers benefit from standardization economies common to any settled convention, and the broader software/hardware ecosystem benefits from interoperability. None of these constitutes a concentrated beneficiary capturing rents from a victim class — which is precisely the structural delta distinguishing this reading from beneficiary_extraction_reading. No victims are declared because, under naturalization, the switching cost borne by anyone choosing to learn a new layout is a private, non-transferred cost of skill acquisition, not an extraction transferred to them by an agenda-setter. Directionality for typists_with_existing_skill sits near symmetric-to-beneficiary (d relatively low) because the status quo actively serves their accumulated investment; directionality for alternative-layout advocates sits moderate (not high) because their cost is opportunity cost of non-adoption, not extraction levied on them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing typebar jams in mechanical typewriters) is definitionally dead — no keyboard sold today has mechanical typebars. Under the naturalization reading, this does NOT indicate mandatrophy/zombie persistence, because the reading's claim is that the layout, whatever its origin, became and remains genuinely adequate on its own terms (trained population size, muscle-memory infrastructure, absence of a demonstrated superior alternative) independent of the original jam-prevention rationale. The mismatch between founding_problem_status=dead and disappearance_verdict is the diagnostic the schema is built to surface: this reading asserts disappearance_verdict is closer to world_unchanged-leaning-contested (removing QWERTY tomorrow would be genuinely disruptive because of real accumulated skill capital, not because a rent-collecting agenda-setter would lose income) — which argues against a capture/zombie flag despite the dead founding problem, precisely because no concentrated beneficiary exists to be the zombie's parasite.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dvorak_advantage_magnitude,
    'Does the Dvorak layout (or any alternative) confer a real, replicable typing-speed or ergonomic advantage over QWERTY large enough to justify switching costs, or is the claimed advantage an artifact of small, methodologically weak, or industry-funded studies (notably the Navy studies commissioned by Dvorak''s own patent holder)?',
    'Independently replicated, blinded, large-N comparative typing studies controlling for training time and typist selection; meta-analysis of the existing literature including reexamination of the original 1944 Navy study''s provenance.',
    'If no robust advantage exists, QWERTY''s persistence is adequately explained by genuine parity plus switching cost — supporting this naturalization reading. If a robust and substantial advantage is confirmed, the persistence looks more like a coordination failure (lock-in reading) regardless of switching costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dvorak_advantage_magnitude, empirical, 'Whether the empirical basis for Dvorak''s claimed superiority is sound or itself a contested/interested artifact.').

omega_variable(
    systematic_beneficiary_absence,
    'Is there any identifiable party that actively benefits from QWERTY''s persistence in a way that would constitute extraction rather than incidental, competitively-earned advantage (e.g., manufacturers with sunk tooling, typing-instruction industries, publishers of QWERTY-trained curricula)?',
    'Trace manufacturer and publisher lobbying/standard-setting history; examine whether any actor has resisted alternative-layout interoperability (e.g., blocking OS-level layout switching) versus merely not subsidizing it.',
    'If a systematic beneficiary with active suppression capacity is found, this constraint is mis-specified as naturalization and should be reclassified toward the beneficiary_extraction_reading sibling. Absent such a beneficiary, the naturalization reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systematic_beneficiary_absence, conceptual, 'Whether the absence of a concentrated beneficiary, central to this reading, survives closer historical scrutiny.').

omega_variable(
    naturalization_vs_lockin_framing,
    'Given identical observable facts (near-universal QWERTY adoption, never-successful Dvorak displacement, real retraining costs), is ''genuine adequacy plus fair competitive lapse'' or ''path-dependent coordination failure'' the correct structural reading — and is this a genuinely empirically resolvable question or an interpretive framing choice that cannot be settled by more data?',
    'Counterfactual/historical case comparison: examine analogous standards contests (e.g., AC vs DC, VHS vs Betamax) where switching DID occur despite incumbency, to calibrate what evidence would distinguish ''adequate and stable'' from ''locked-in and suboptimal.''',
    'If the two readings are empirically indistinguishable in principle, the kernel is irreducibly under-determined and both readings persist as live interpretive commitments rather than one being correctable to the other.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalization_vs_lockin_framing, conceptual, 'Whether the naturalization/lock-in dispute is empirically resolvable or a permanent framing divide — this constraint''s central Ω_C.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__naturalization_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(qwer_tr_t20, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 20, 0.06).
narrative_ontology:measurement(qwer_tr_t40, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(qwer_tr_t70, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 70, 0.08).
narrative_ontology:measurement(qwer_tr_t100, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 100, 0.09).
narrative_ontology:measurement(qwer_tr_t140, qwerty_persistence_mechanism__naturalization_reading, theater_ratio, 140, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qwer_be_t20, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(qwer_be_t40, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(qwer_be_t70, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 70, 0.11).
narrative_ontology:measurement(qwer_be_t100, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 100, 0.12).
narrative_ontology:measurement(qwer_be_t140, qwerty_persistence_mechanism__naturalization_reading, base_extractiveness, 140, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(qwerty_persistence_mechanism__naturalization_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__naturalization_reading, information_standard).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_mechanism__naturalization_reading, 0.03).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__lock_in_reading).
narrative_ontology:affects_constraint(qwerty_persistence_mechanism__naturalization_reading, qwerty_persistence_mechanism__beneficiary_extraction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language claim 'the QWERTY conjecture' (per the eps-invariance principle). naturalization_reading (this file, epsilon=0.12, claimed rope) asserts genuine adequacy and fair competitive lapse of alternatives. lock_in_reading asserts path-dependent coordination failure despite technical inferiority (expected higher epsilon, likely tangled_rope or piton). beneficiary_extraction_reading asserts active incumbent suppression protecting training/market investments (expected highest epsilon, likely snare or tangled_rope, with named beneficiaries and victims). All three share the same observable history but diverge in claimed beneficiary structure and epsilon; they are linked here rather than merged because measuring the same historical facts under different beneficiary-attribution assumptions yields structurally distinct constraints, not one constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
