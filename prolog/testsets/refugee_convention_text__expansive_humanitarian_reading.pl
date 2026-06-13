% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention as Expansive Humanitarian Mandate
 *   domain: international_law/migration/human_rights
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested Refugee
 *   Convention kernel. The expansive humanitarian reading interprets
 *   'well-founded fear of persecution' to include systematic generalized
 *   violence and non-state persecution, and 'particular social group' to
 *   include gender, sexual orientation, and clan-based persecution. This
 *   reading centers the Convention's humanitarian object and the framers'
 *   intent to prevent the 1951 refusal-to-protect pattern. The constraint's
 *   operation requires signatory states to assess asylum claims
 *   substantively, prohibits offshore processing and interdiction as
 *   refoulement violations, and creates protection duties for vulnerable
 *   populations. Sibling readings (restrictive sovereignty and procedural
 *   integrity) instantiate different interpretive frameworks of the same text
 *   and are modeled as separate constraints with their own ε values and
 *   structural data. This reading's ε is modest (0.32) because its operation
 *   coordinates international humanitarian norms rather than extracting from
 *   powerless parties—the burden on sovereigns is real but justified by the
 *   humanitarian mandate the reading instantiates.
 *
 * KEY AGENTS:
 *   - Expansive humanitarian advocates (organized; set the interpretive agenda)
 *   - Persecuted persons from generalized violence (powerless; trapped exit; the broadened victim set)
 *   - LGBTQ+ persons fleeing persecution (moderate power; identity-locked; protected as social group)
 *   - Clan-based persecution victims (powerless; constrained exit; protected under particular social group)
 *   - State asylum sovereigns (institutional; bear fiscal and administrative costs)
 *   - Restrictive sovereigntist governments (excluded; would deny broad readings)
 *   - Procedural integrity advocates (excluded; center process over breadth)
 *   - International courts/treaty bodies (analytical seats; measure interpretive coherence)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.32).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.18).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention as Expansive Humanitarian Mandate").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration/human_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, 'fa071443-76ff-45ba-8b9a-cef3acfd51b7').
narrative_ontology:cs_kernel_codification('fa071443-76ff-45ba-8b9a-cef3acfd51b7', fixed_text).
narrative_ontology:cs_authority_grounding('fa071443-76ff-45ba-8b9a-cef3acfd51b7', lineage).
narrative_ontology:cs_interpretation_layer_present('fa071443-76ff-45ba-8b9a-cef3acfd51b7').
narrative_ontology:cs_reading_relation('fa071443-76ff-45ba-8b9a-cef3acfd51b7', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa071443-76ff-45ba-8b9a-cef3acfd51b7', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('fa071443-76ff-45ba-8b9a-cef3acfd51b7', foundational, humanitarian_obligations_non_negotiable).
narrative_ontology:cs_axiom_status(humanitarian_obligations_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('fa071443-76ff-45ba-8b9a-cef3acfd51b7', humanitarian_obligations_non_negotiable, deontological).
narrative_ontology:cs_axiom('fa071443-76ff-45ba-8b9a-cef3acfd51b7', foundational, persecution_broadly_construed).
narrative_ontology:cs_axiom_status(persecution_broadly_construed, holdable).
narrative_ontology:cs_axiom_grounding('fa071443-76ff-45ba-8b9a-cef3acfd51b7', persecution_broadly_construed, conventional).
narrative_ontology:cs_reference_frame('fa071443-76ff-45ba-8b9a-cef3acfd51b7', humanitarian_obligations_non_negotiable).
narrative_ontology:cs_drift_state('fa071443-76ff-45ba-8b9a-cef3acfd51b7', contemporary_offshore_processing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa071443-76ff-45ba-8b9a-cef3acfd51b7', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, persecuted_persons_from_generalized_violence).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, lgbtq_plus_persons_fleeing_persecution).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, persons_in_particular_social_groups).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, non_state_persecution_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_victims).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, state_asylum_sovereigns).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, humanitarian_law_supremacy).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, dignity_preservation_mandate).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, asylum_as_human_right_not_privilege).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International human rights organizations, refugee advocacy networks, and judges/jurists in constitutional/human rights courts who read the Convention's text as a binding humanitarian floor that protects the broadest vulnerable populations. They argue 'well-founded fear' must encompass generalized violence and gang/criminal persecution when systematic, and 'particular social group' includes gender-based, sexual-orientation-based, and clan-based persecution. They set the interpretive agenda through litigation, advisory opinions, and normative advocacy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, expansive_humanitarian_advocates, agenda_setter,
    organized, generational, mobile, global).

% Flee regions experiencing widespread armed conflict, gang violence, or state collapse where civilian populations face generalized deadly risk without individualized targeting. Under this reading they qualify for protection; under restrictive readings they must prove personalized persecution, which is often impossible in chaos. Their exit from the country is their only safety option.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, persecuted_persons_from_generalized_violence, beneficiary,
    powerless, immediate, trapped, global).

% Face state or social persecution for sexual orientation or gender identity in jurisdictions where such persecution is legal or culturally endemic. Their identity cannot be set aside; exit requires leaving the country. This reading protects them as members of a 'particular social group'; restrictive readings may deny that group membership is grounds for protection.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, lgbtq_plus_persons_fleeing_persecution, beneficiary,
    moderate, biographical, identity_locked, global).

% Face persecution based on family or clan affiliation in societies where clan identity determines resource access, legal standing, and safety. They are persecuted not for individual conduct but for clan membership. This reading recognizes clan as a 'particular social group'; restrictive readings may exclude it as non-immutable.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, clan_based_persecution_victims, beneficiary,
    powerless, biographical, constrained, regional).

% Implement the Convention through asylum systems, and bear the cost (fiscal, social, security screening) of expanded protections. They argue that broad readings increase burdens and that migration policy should remain under sovereign control. Under this reading, they cannot use procedural barriers or offshore processing to avoid substantive assessment obligations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, state_asylum_sovereigns, payer,
    institutional, generational, mobile, national).

% Explicitly reject broad interpretations of the Convention and argue for maximum discretion to define 'persecution,' limit 'particular social group,' and prioritize border control. They would argue that 'well-founded fear' must be individually proven, not inferred from generalized conditions. They are structurally excluded from shaping this reading's framework because they deny the reading's foundational premise.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictive_sovereigntist_governments, excluded,
    institutional, generational, mobile, national).

% Focus on fair, consistent, and transparent asylum adjudication processes rather than on expanding substantive protection definitions. They would argue that outcome breadth matters less than procedural integrity—that a narrow but fairly applied definition is preferable to a broad definition applied inconsistently. They are excluded from this reading because they do not center the humanitarian mandate as the primary frame.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, procedural_integrity_advocates, excluded,
    organized, generational, mobile, global).

% Interpret and apply the Convention through case law and advisory opinions. They synthesize state practice, scholarly interpretation, and changing circumstances to determine whether the reading's claims are consistent with the text's ordinary meaning and object/purpose. They serve as analytical seats measuring the coherence of the constraint.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_courts_and_treaty_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a uniform international standard for asylum protection that coordinates sovereign asylum systems around a common humanitarian baseline: asylum law becomes comparable across jurisdictions, vulnerable populations receive predictable protection, and the Convention's framers' intent to protect the displaced and persecuted is honored across signatories.
% TRANSFER_FUNCTION: Moves the burden and responsibility of protection from the persecuted persons themselves (who must find private sponsors, navigate underground routes, or face death) to the international community via signatory states. The cost transfers to asylum-implementing states as fiscal and administrative burden; the benefit transfers to vulnerable persons as access to safety and asylum status.
% ABSENT_VOICES: Restrictive sovereigntist governments that explicitly reject this reading are structurally excluded from the humanitarian frame; they would argue for narrow definitions and maximum discretion but are not participants in the expansive interpretation consensus. Procedural-integrity advocates who would emphasize process over outcome breadth are also practically absent from the advocacy coalition driving this reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished and the restrictive sovereigntist reading dominated globally, millions of people currently protected (those fleeing generalized violence, LGBTQ+ persons in persecuting societies, clan-persecuted persons) would lose asylum access and face return to harm. The entire humanitarian asylum framework that has accumulated since the Convention's signing would reorganize around narrower definitions, state control would expand, and the protected population would shrink dramatically. The reading's disappearance would be an institutional sea change.
% FOUNDING_PROBLEM: In 1951, the framers drafted the Convention to prevent a repeat of WWII-era refusals to grant asylum to persecuted populations. They intended to create an international legal barrier against returning people to persecution, defined broadly to encompass war, political persecution, and group-based harm. The founding problem was: how do we ensure no sovereign can unilaterally decide that a persecuted population has 'no problem' when persecution is systematic and deadly?
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (Marrus & Paxton on WWII refugee crises; Loescher on the Convention's drafting history) documents the explicit intention to prevent state discretion from allowing persecution. The UN Handbook on Procedures and Criteria (written by the UNHCR, the Convention's custodian body) endorses the expansive reading. Decisions of the ICJ, ECHR, and national constitutional courts in human rights traditions cite the Convention's humanitarian object/purpose and interpret it expansively. Restrictive sovereigntist governments attest the problem is 'solved' and the Convention's scope is excessive—but their attestation is from the party that would reduce protection, not from neutral analysts. External corroboration (academic international law, human rights monitoring organizations, countries with strong asylum jurisprudence) attests the founding problem remains live and the expansive reading honors the framers' intent.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.32) because the reading does not transfer wealth or rents to a concentrated beneficiary; instead it imposes humanitarian duties on states to protect defined beneficiaries. The beneficiaries (persecuted persons) do not 'pay' the payer (states) in exchange; the flow is unidirectional humanitarian obligation. Suppression is very low (0.18) because the reading is not maintained coercively—it is sustained through normative agreement, treaty obligation, and judicial interpretation. Theater is minimal (0.12) because the reading's core function (protecting vulnerable populations) is the actual work being done; there is little performative maintenance. Accessibility collapse is high (0.78) because once the humanitarian frame is accepted, alternatives (narrow definitions, state discretion, refoulement) become logically incoherent within the reading's framework—the humanitarian mandate closes off those moves. Resistance is high (0.71) because restrictive sovereigntist governments actively resist this reading and push back through policy, litigation, and legislative amendments (non-refoulement carve-outs, offshore processing regimes, 'safe third country' doctrines). The measurement series shows extractiveness and suppression remaining stable over the interval, with theater ratio similarly stable—this is a mature interpretive frame with durable normalized operation.
 *
 * PERSPECTIVAL GAP:
 *   From the expansive humanitarian advocates' and persecuted persons' seats, the reading is a necessary humanitarian floor that prevents catastrophic harm. From state asylum sovereigns' seats, the reading imposes significant fiscal and administrative burdens without corresponding discretion. From restrictive sovereigntist governments' seats, the reading is an illegitimate constraint on sovereignty and an overreach of treaty text. From international courts' seats, the reading is a coherent interpretation of the Convention's text and object/purpose, supported by the Vienna Convention on the Law of Treaties. The engine computes these divergences from the structural data (power, exit options, beneficiary/payer position). This reading does not adjudicate the divergence—it instantiates one position within it.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted persons are the primary beneficiaries (d near 0.0: they receive protection without paying; exit is trapped, making the protection benefit substantial). State sovereigns are the primary payers (d near 1.0: they bear fiscal/administrative costs; exit is mobile but choosing not to comply creates legal and reputational consequences). Expansive humanitarian advocates are the agenda-setters (d near 0.5: they benefit from the reading being accepted but also incur advocacy costs). The reading's operation moves directional asymmetry upward for persecuted populations and downward for sovereigns—the humanitarian mandate inverts the default power gradient by imposing duties on the powerful to protect the powerless. Suppression is low because the reading's persistence depends on normative agreement and judicial interpretation, not on coercion—states that accept the humanitarian frame willingly implement it; states that resist do so openly through policy contestation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing return to persecution, ensuring asylum access) remains live—humanitarian crises, generalized violence, gender-based and LGBTQ+ persecution all continue. The reading's function is not atrophied; it is actively invoked in litigation, policy, and jurisprudence. However, there is tension between the reading's stated mandate (broad humanitarian protection) and its actual operation (significant restrictions through offshore processing, interdiction, safe-third-country doctrines, and asylum seeker criminalization). This is not mandatrophy—the constraint has not atrophied into pure performance—but it is evidence of counter-readings actively suppressing the humanitarian mandate's reach. The theater ratio is low because the protective function is genuine, but the suppression ratio is high because states implement physical/procedural barriers that reduce the reading's practical effect. This profile is consistent with a contested rope that is losing institutional compliance: the humanitarian obligation persists in law, but its enforcement mechanism (substantive assessment, protection duties) is degraded through procedural circumvention.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalized_violence_vs_individualized_persecution,
    'Does ''persecution'' require individualized state targeting, or can systematic generalized violence (war, gang violence, state collapse) constitute persecution even without individual identification?',
    'International jurisprudence accumulation (ICJ, ECHR, national constitutional courts) via case law that establishes whether generalized violence contexts grant asylum. Empirical documentation of survival rates and harm patterns in conflict-affected regions.',
    'If generalized violence counts as persecution, the beneficiary population expands dramatically (millions in war zones). If it does not, the beneficiary population shrinks to those who can prove individual targeting, excluding most people fleeing armed conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalized_violence_vs_individualized_persecution, conceptual, 'Whether persecution requires individual targeting or encompasses systematic generalized harm.').

omega_variable(
    non_state_actor_persecution_scope,
    'Does ''persecution'' require state involvement, or can persecution by non-state actors (gangs, militias, family, private persons) trigger asylum obligations?',
    'Treaty interpretation via Vienna Convention on the Law of Treaties (object/purpose analysis); state practice documentation showing whether non-state persecution is recognized in asylum adjudication; empirical evidence of harm from non-state actors where state capacity to protect is absent.',
    'If non-state persecution counts, victims of gang violence, domestic abuse, and private discrimination gain protection. If it does not, only those persecuted by states qualify, excluding millions facing private violence in weak-governance contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_state_actor_persecution_scope, conceptual, 'Whether asylum protection extends to persecution by non-state actors or only state actors.').

omega_variable(
    particular_social_group_immutability_vs_experience,
    'Does ''particular social group'' require immutable characteristics (race, ethnicity, gender at birth) or can it include mutable characteristics that form lived identity (sexual orientation, gender identity, clan membership)?',
    'Jurisprudential synthesis across jurisdictions with different standards (Canada''s ''social visibility'' test, EU''s approach to gender-based persecution, US Board of Immigration Appeals decisions). Empirical evidence of the actual basis of persecution (whether perpetrators target immutable traits or lived identities).',
    'If mutable characteristics count, LGBTQ+ persons, people in particular clans, and those with acquired group membership gain protection. If immutability is required, sexual orientation and gender identity may be excluded, and clan-based persecution may not qualify.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(particular_social_group_immutability_vs_experience, conceptual, 'Whether ''particular social group'' includes mutable group memberships or requires immutable characteristics.').

omega_variable(
    humanitarian_mandate_vs_sovereigntist_containment,
    'Is the Refugee Convention''s core function humanitarian protection (the reading''s claim) or sovereignty-constrained burden-sharing with discretion (the restrictive reading''s claim)?',
    'Historical analysis of the Convention''s drafting debates, framers'' explicit statements of intent, and the text''s object/purpose under Vienna Convention rules. State practice documentation: do states implement the Convention as humanitarian mandate or as discretionary floor?',
    'If humanitarian mandate is correct, the Convention is an unbendable obligation to protect broad classes of vulnerable persons, and restrictive doctrines (offshore processing, interdiction, safe-third-country rules) are violations. If sovereigntist containment is correct, states retain discretion and procedural barriers are legitimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_mandate_vs_sovereigntist_containment, conceptual, 'Whether the Convention is a humanitarian mandate or a sovereignty-constrained burden-sharing agreement.').

omega_variable(
    reading_foreclosure_via_axiom_overriding,
    'Has the humanitarian_obligations_non_negotiable axiom (foundational to this reading) been empirically or politically overridden by state practice in offshore processing, interdiction, and safe-third-country regimes?',
    'Documentation of state adoption of non-refoulement-violating practices (offshore processing, interdiction at sea, safe-third-country agreements) and whether courts or treaty bodies explicitly override the humanitarian axiom or merely constrain its application.',
    'If the axiom has been overridden, this reading is foreclosed in practical operation and the restrictive sovereignty reading dominates de facto. If courts have merely applied the axiom with flexibility (recognizing procedural exceptions while preserving the substance), the reading persists as a contested norm.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_via_axiom_overriding, empirical, 'Whether state practice in offshore processing and interdiction has empirically foreclosed the humanitarian mandate axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(refu_tr_t0, observed).
narrative_ontology:measurement(refu_tr_t5, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(refu_tr_t5, observed).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(refu_tr_t10, observed).
narrative_ontology:measurement(refu_tr_t15, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(refu_tr_t15, observed).
narrative_ontology:measurement(refu_tr_t25, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(refu_tr_t25, observed).
narrative_ontology:measurement(refu_tr_t35, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 35, 0.12).
narrative_ontology:measurement_basis(refu_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(refu_be_t0, observed).
narrative_ontology:measurement(refu_be_t5, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 5, 0.29).
narrative_ontology:measurement_basis(refu_be_t5, observed).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(refu_be_t10, observed).
narrative_ontology:measurement(refu_be_t15, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement_basis(refu_be_t15, observed).
narrative_ontology:measurement(refu_be_t25, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement_basis(refu_be_t25, observed).
narrative_ontology:measurement(refu_be_t35, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 35, 0.32).
narrative_ontology:measurement_basis(refu_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(refu_su_t0, observed).
narrative_ontology:measurement(refu_su_t5, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 5, 0.16).
narrative_ontology:measurement_basis(refu_su_t5, observed).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement_basis(refu_su_t10, observed).
narrative_ontology:measurement(refu_su_t15, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 15, 0.18).
narrative_ontology:measurement_basis(refu_su_t15, observed).
narrative_ontology:measurement(refu_su_t25, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement_basis(refu_su_t25, observed).
narrative_ontology:measurement(refu_su_t35, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 35, 0.18).
narrative_ontology:measurement_basis(refu_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_principle__expanded_scope).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, offshore_processing_regimes).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel decomposes into three separate constraints: expansive_humanitarian_reading (this constraint), restrictive_sovereignty_reading, and procedural_integrity_reading. Each reading interprets the same formalized text (the Convention) but instantiates a structurally different constraint with different ε values, beneficiary/victim structures, and claimed types. The readings are linked via this network field to indicate kernel family membership. All three readings must be authored to fully capture the constraint family; each reading's ε is independently authored and should not be reconciled across readings. The sibling readings are NOT variants of this constraint—they are separate constraints modeling alternative interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__expansive_humanitarian_reading, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
