% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems: Sovereignty Primacy Reading (National Security Override Framework)
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   The 'One Country, Two Systems' framework is a constitutional-level
 *   commitment (the Hong Kong Basic Law, itself the instrument of the PRC's
 *   sovereignty claim) that operates at two structural levels: (1) as a
 *   coordinating mechanism that unified Hong Kong and mainland China under
 *   one sovereignty while preserving distinct systems, and (2) as an
 *   authority-hierarchy mechanism that places ultimate sovereignty with the
 *   mainland and derives Hong Kong autonomy from delegated authority that can
 *   be revoked. This story instantiates the sovereignty_primacy reading:
 *   autonomy is delegated, revocable, and subordinate to mainland definitions
 *   of national security and territorial integrity. The alternative readings
 *   (autonomy_primacy, balanced_coexistence) interpret the same Basic Law
 *   kernel to prioritize different values and authority structures. This
 *   reading's concrete instantiation is the National Security Law (2020),
 *   which carves national security matters from Hong Kong's autonomous
 *   jurisdiction and stations mainland enforcement agents in Hong Kong
 *   territory.
 *
 * KEY AGENTS:
 *   - Mainland Central Authority: ultimate sovereign, sets the framework
 *   - Mainland State Security Apparatus: direct beneficiary, expanded enforcement jurisdiction
 *   - Hong Kong Government: delegated administrator, constrained executor
 *   - Hong Kong Judiciary: structurally compromised, identity-locked on security matters
 *   - Hong Kong Citizens: trapped payers, political speech now high-epsilon
 *   - Civil Liberties Advocates: moderately powered but suppressed, face prosecution risk
 *   - International Observers: excluded, structurally external to the framework
 *   - Autonomy-Primacy Advocates: excluded AND prosecutable under the constraint itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.81).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.89).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems: Sovereignty Primacy Reading (National Security Override Framework)").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, 'e7fb95fe-775f-4b86-8aaf-f4646a67517c').
narrative_ontology:cs_kernel_codification('e7fb95fe-775f-4b86-8aaf-f4646a67517c', formalized).
narrative_ontology:cs_authority_grounding('e7fb95fe-775f-4b86-8aaf-f4646a67517c', extraction).
narrative_ontology:cs_interpretation_layer_present('e7fb95fe-775f-4b86-8aaf-f4646a67517c').
narrative_ontology:cs_reading_relation('e7fb95fe-775f-4b86-8aaf-f4646a67517c', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('e7fb95fe-775f-4b86-8aaf-f4646a67517c', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('e7fb95fe-775f-4b86-8aaf-f4646a67517c', foundational, mainland_sovereign_authority_primacy).
narrative_ontology:cs_axiom_status(mainland_sovereign_authority_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e7fb95fe-775f-4b86-8aaf-f4646a67517c', mainland_sovereign_authority_primacy, deontological).
narrative_ontology:cs_axiom('e7fb95fe-775f-4b86-8aaf-f4646a67517c', foundational, autonomy_is_delegated_and_revocable).
narrative_ontology:cs_axiom_status(autonomy_is_delegated_and_revocable, holdable).
narrative_ontology:cs_axiom_grounding('e7fb95fe-775f-4b86-8aaf-f4646a67517c', autonomy_is_delegated_and_revocable, deontological).
narrative_ontology:cs_axiom('e7fb95fe-775f-4b86-8aaf-f4646a67517c', secondary, national_security_overrides_local_autonomy).
narrative_ontology:cs_axiom_status(national_security_overrides_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('e7fb95fe-775f-4b86-8aaf-f4646a67517c', national_security_overrides_local_autonomy, instrumental).
narrative_ontology:cs_reference_frame('e7fb95fe-775f-4b86-8aaf-f4646a67517c', mainland_sovereign_primacy_framework).
narrative_ontology:cs_drift_state('e7fb95fe-775f-4b86-8aaf-f4646a67517c', post_national_security_law_enforcement_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e7fb95fe-775f-4b86-8aaf-f4646a67517c', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_central_authority).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, civil_liberties_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, international_business_community).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sovereign authority over Hong Kong and sets the framework through which autonomy is delegated. Controls the National Security Law and its interpretation, unilaterally defines what constitutes threats to national security and territorial integrity, and reserves the right to override local autonomy decisions. Does not participate directly in Hong Kong governance day-to-day but establishes the ultimate decision authority.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_central_authority, agenda_setter,
    institutional, civilizational, analytical, national).

% Expands enforcement jurisdiction into Hong Kong through the National Security Law; operates mainland security agents within Hong Kong territory; gains authority to investigate and prosecute political speech, assembly, and separatist activity without Hong Kong judicial constraints. Benefits from the constraint by acquiring expanded territorial security capacity and political control.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus, agenda_setter).

% Administers local affairs within the delegated autonomy framework but must execute mainland authority's security directives and enforce the National Security Law as written by the mainland. Retains administrative functions but has lost effective veto power over security matters. Positioned as both enforcer of the constraint and subject to it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_government, payer,
    institutional, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_government, beneficiary).

% Bound by the One Country, Two Systems framework to maintain independence, but the National Security Law carves out security matters from its jurisdiction. Security-related prosecutions fall under mainland interpretation and authority. Judicial independence on ordinary commercial and civil matters persists, but the most politically contentious cases are removed from local adjudication. Identity as an independent common-law judiciary is structurally compromised on the constraint's most extractive dimension.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    institutional, generational, identity_locked, local).

% Subject to the National Security Law and mainland enforcement agents operating in Hong Kong. Political speech, assembly, and protest activity now carry the risk of mainland prosecution. Exit options are emigration (capital-intensive, identity-wrenching), internal exit (self-censorship, depoliticization), or acceptance of the legal jeopardy. Trapped because leaving requires abandoning networks, property, and professional identity.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens, payer,
    powerless, biographical, trapped, local).

% Advocate for judicial independence and civil liberties protection in Hong Kong but face prosecution risk for speech and organizing activity under the National Security Law. Cannot effectively organize at scale (suppression blocks coordination) and face mainland prosecution for international advocacy. Exit to diaspora communities is open but carries professional and relational costs.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, civil_liberties_advocates, payer,
    moderate, biographical, constrained, global).

% UN bodies, human rights organizations, and foreign governments observe and critique the constraint but cannot enforce alternative readings. Mainland authority rejects external interference as colonial-era thinking and territorial violation. Their objections are structurally external to the constraint's operation.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_observers, excluded,
    powerful, generational, constrained, global).

% Hold an alternative reading of One Country, Two Systems that centers Hong Kong autonomy as a treaty commitment and checks on mainland authority. They are excluded from the dominant institutional framing because the sovereignty_primacy reading has consolidated authority control. Advocacy for their reading itself becomes prosecutable speech under the National Security Law.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, autonomy_primacy_advocates, excluded,
    moderate, biographical, trapped, local).

% Segments that depend on mainland market access benefit from the clarity of mainland authority's security framework and the suppression of political disruption risk. They enjoy protected commercial operation in Hong Kong (taxation, contract enforcement) while political speech is constrained. Can exit to other financial centers but benefit from remaining within the Hong Kong/mainland nexus.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_business_community, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_state_security_apparatus).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes unified security jurisdiction and conflict-resolution authority: when security concerns arise, mainland authority has final decision power rather than requiring negotiation between systems. Eliminates institutional stalemate and coordinates political control within a single sovereignty hierarchy.
% TRANSFER_FUNCTION: Transfers political authority, enforcement jurisdiction, and legal sovereignty from Hong Kong institutions (judiciary, legislature, executive) to mainland state apparatus. Moves the locus of definition for national security and territorial integrity from local to mainland interpretation. Moves enforcement capacity from local to mainland agents operating in Hong Kong territory.
% ABSENT_VOICES: International observers, foreign governments, and local autonomy-primacy advocates are excluded from the institutional reading of the constraint. They assert treaty guarantees, civil liberties primacy, and the original intention of meaningful autonomy — but the sovereignty_primacy reading brackets these voices as external interference or colonial nostalgia. Their exclusion is structural: the constraint operates to suppress the advocacy that would represent these voices.
% DISAPPEARANCE_RATIONALE: If this sovereignty-override framework disappeared, Hong Kong would immediately recover judicial independence on security matters, local legislatures would regain veto power, and the mainland security apparatus would lose territorial enforcement capacity. Political speech and assembly activity would no longer carry mainland prosecution risk. The reallocation of institutional power would be immediate and fundamental.
% FOUNDING_PROBLEM: Protecting Chinese territorial integrity and national security from perceived separatist movements in Hong Kong; preventing foreign interference in Hong Kong institutions under the guise of autonomy; maintaining political control as Hong Kong developed a distinct political identity from the mainland after 1997.
% FOUNDING_PROBLEM_CORROBORATION: Mainland authorities attest the founding problem is live and intensifying: evidence of foreign support for Hong Kong protest movements, legislative calls for independence in international forums, and political organizing they characterize as separatist. International observers and Hong Kong autonomy advocates attest the founding problem has been substantially reframed: the mainland characterizes ordinary protest and civil liberties advocacy as security threats, and the response (National Security Law enforcement) has become more extractive than the original security concern. The Post-2019 escalation record (from extradition bill protests to NSL passage) is cited as evidence both that the security concern was real and that the remedy has become political suppression.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.81) and rising because the constraint transfers political authority and enforcement capacity from local institutions to mainland apparatus, and the scope of that transfer has expanded over the measurement interval as prosecutions have widened and judicial carve-outs have deepened. Suppression is very high (0.89) because the constraint persists through active enforcement: prosecution of speech-related activity, detention of activists, institutional pressure on civil society organizations, and the implicit threat of mainland enforcement agent presence. Suppression is NOT declining because the constraint remains new (National Security Law passed 2020) and enforcement capacity is still being built out. Theater_ratio rises moderately (0.18 to 0.42) because while the security function is real (Hong Kong protest movements were internationally organized and politically disruptive), an increasing share of NSL prosecutions target ordinary speech and assembly activity that Hong Kong law would not have criminalized — the theater component (the formal legitimation of what is fundamentally political control) grows as the constraint's function shifts from specific security incidents to generalized political suppression. Accessibility_collapse is high (0.78) because once citizens understand that certain speech/assembly activities trigger NSL prosecution risk, alternatives (self-censorship, emigration, depoliticization) become salient; but the collapse is not total because the international business community and political actors with mainland connections can navigate between constraint and exit. Resistance is substantial (0.72) because protest movements, civil liberties litigation, and international advocacy persist despite suppression — the constraint is not yet so mature that resistance has atrophied.
 *
 * PERSPECTIVAL GAP:
 *   The mainland_central_authority and hong_kong_government seats compute very differently from this reading. Mainland authority experiences the constraint as successful coordination: national security is unified, political control is consolidated, sovereignty is preserved. The Hong Kong Government experiences it as constrained administration: they retain visible power but lack final authority. Citizens and advocates experience it as coercive suppression. The Hong Kong Judiciary experiences it as a threat to professional identity: judges are common-law trained and constitutionally independent, yet NSL security cases are removed from their purview. These gaps should be computed by the engine from the structural data: the same constraint, experienced from different seats, produces different effective extraction profiles. This story's authored metrics describe the constraint FROM THE SOVEREIGNTY_PRIMACY READING'S OWN LIGHTS — the high extractiveness and suppression are how this reading SEES the situation (mainland authority leveraging control, suppressing alternatives); an autonomy_primacy reading would author lower extractiveness on the same events because it would interpret mainland actions as illegitimate power grabs rather than legitimate security operations.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland Central Authority and the State Security Apparatus are full beneficiaries (d near 0.0): they gain jurisdiction, enforcement capacity, and political control without bearing the constraint's costs. Hong Kong Government sits near symmetric-to-payer (d ≈ 0.45-0.55): it retains administrative functions and tax revenue but has lost sovereignty on matters it once controlled, and it must enforce policies it did not author. Hong Kong Citizens and Civil Liberties Advocates are full targets (d near 1.0): they bear suppression, prosecution risk, and lost political agency. Hong Kong Judiciary is notably positioned at d ≈ 0.60-0.70 (substantial target but with identity-lock: judges are professionally bound to interpret and administer law, which locks them into the role of enforcer of a constraint that constrains their own jurisdiction). The directionality derivation here is complicated by the reading's own structure: this is a sovereignty_primacy reading that ASSERTS mainland authority as legitimate, so the 'agenda_setter' role for mainland authority is not a neutral classification but the reading's own normative stance. An autonomy_primacy reading would classify mainland authority very differently (as a usurper, as a violator, as outside legitimate authority). The directionality is reading-indexed, as stated in the ε referent guidance: the reading's endorsed authority structure shapes who is beneficiary and who is victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting territorial integrity from separatism; preventing foreign interference) is declared 'contested' because the scale of the problem and the appropriateness of the response are in dispute. The mainland characterizes the post-2019 protest movement as separatist and foreign-influenced; autonomy advocates characterize the same movement as legitimate civil-liberties activism responding to a perceived erosion of promised autonomy. The disappearance verdict ('world_rearranges') is compatible with the sovereignty_primacy reading: if the constraint disappeared, mainland authority would lose enforcement capacity and political control. But a mandatrophy reading would ask: has the founding problem itself disappeared or transformed? The measurement series shows extraction rising over time even as the specific security incidents (2019 protests) recede into the past. This pattern — extraction persisting and even intensifying after the original problem is nominally resolved — is the mandatrophy signature. The sovereignty_primacy reading does not admit mandatrophy because it ASSERTS that territorial integrity and national security remain live and require permanent institutional control. An autonomy_primacy reading would see mandatrophy here: the infrastructure built to address a crisis persists and expands even as the crisis wanes, extracting political authority with no sunset condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_threat_definition_ambiguity,
    'What constitutes a legitimate ''national security and territorial integrity'' threat sufficient to override local autonomy? Is the criterion the presence of objective separatist intent, or the mainland authority''s declaration that such intent exists?',
    'Comparison of prosecutions across cases: do NSL prosecutions target demonstrable separatist organizing, or do they target speech and assembly activity that would be protected in other common-law jurisdictions? Post-NSL empirical analysis of charge categories and conviction grounds.',
    'If prosecutions target only objective separatism, the constraint is narrower than authored; if they target normative political speech, the extractiveness is understated and the suppression is more purely political than security-justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_threat_definition_ambiguity, empirical, 'Whether security-override authority is bounded by objective criteria or exercised at mainland discretion.').

omega_variable(
    reading_contest_justiciability,
    'Can the sovereignty_primacy reading be juridically foreclosed by the autonomy_primacy reading, or do they coexist as live political positions regardless of legal adjudication?',
    'Hong Kong court ruling on the scope of NSL, or international tribunal ruling on Basic Law interpretation, would formally adjudicate between readings. But even judicial foreclosure of one reading might not displace the other as a live political claim.',
    'If one reading can be juridically foreclosed, the constraint type depends on which reading prevails. If readings coexist indefinitely despite judicial rulings, they remain rival constraint-producing interpretations and mandatrophy clock remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_justiciability, conceptual, 'Whether kernel readings are subject to definitive legal resolution or persist as political dispute.').

omega_variable(
    mainland_enforcement_capacity_sustainability,
    'Does mainland enforcement authority in Hong Kong depend on international acquiescence, domestic consent, or institutional autonomy such that either source can erode independently?',
    'Observation of enforcement capacity over the next 5-10 years: does it expand or plateau? Do international sanctions or local resistance measurably constrain it? Can the mainland operate NSL enforcement without significant defection from Hong Kong institutions?',
    'If enforcement depends on Hong Kong institutional cooperation, rising resistance could degrade suppression capacity. If enforcement is independent (mainland agents, mainland courts), it persists despite resistance. This determines whether the high suppression is sustainable or approaching a stability threshold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mainland_enforcement_capacity_sustainability, empirical, 'Whether mainland enforcement relies on Hong Kong institutional integration or operates independently.').

omega_variable(
    identity_lock_mechanism_on_judiciary,
    'Will Hong Kong judges remain bound by professional identity to administer the NSL (even while their jurisdiction is carved out), or will the professional identity itself shatter under the structural contradiction?',
    'Observation of judicial conduct on security cases, mass resignations or refusals to serve, or explicit rejection of the NSL''s legitimacy by the judiciary as a body.',
    'If identity-lock persists, the judiciary remains partially captured (enforcing ordinary law while NSL enforcement is external). If identity-lock breaks, the judiciary could become a point of institutional resistance and slow enforcement through procedural objections and interpretive gaps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_on_judiciary, empirical, 'Whether professional judicial identity will sustain cooperation with NSL enforcement or fracture under institutional constraint.').

omega_variable(
    alternative_kernel_readings_viability,
    'Is the autonomy_primacy reading still a live institutional position, or has the sovereignty_primacy reading consolidated so completely that the alternative reading is prosecutable speech rather than a legitimate governance claim?',
    'Observation of whether autonomy-primacy advocates are prosecuted for articulating their reading, or whether the reading persists as an unchallenged alternative framing within Hong Kong institutions or international forums.',
    'If autonomy_primacy reading is prosecuted as sedition, the kernel becomes uncontested (sovereignty_primacy has foreclosed its rivals). If the reading persists, the kernel remains contested and the constraint remains subject to competing interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_kernel_readings_viability, empirical, 'Whether alternative kernel readings remain viable or have been prosecuted into silence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(one__tr_t0, projected).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(one__tr_t5, observed).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(one__tr_t10, observed).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(one__tr_t15, observed).
narrative_ontology:measurement(one__tr_t20, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(one__tr_t20, observed).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(one__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(one__be_t0, projected).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(one__be_t5, observed).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(one__be_t10, observed).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 15, 0.77).
narrative_ontology:measurement_basis(one__be_t15, observed).
narrative_ontology:measurement(one__be_t20, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement_basis(one__be_t20, observed).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement_basis(one__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(one__su_t0, projected).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 5, 0.73).
narrative_ontology:measurement_basis(one__su_t5, observed).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 10, 0.81).
narrative_ontology:measurement_basis(one__su_t10, observed).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 15, 0.86).
narrative_ontology:measurement_basis(one__su_t15, observed).
narrative_ontology:measurement(one__su_t20, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 20, 0.88).
narrative_ontology:measurement_basis(one__su_t20, observed).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 25, 0.89).
narrative_ontology:measurement_basis(one__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(one_country_two_systems_framework__sovereignty_primacy_reading, 0.25).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judicial_independence_constraint).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_political_speech_constraint).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_civil_assembly_constraint).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested One Country, Two Systems kernel (Hong Kong Basic Law). The sovereignty_primacy reading interprets autonomy as delegated and revocable; the autonomy_primacy_reading interprets autonomy as treaty-guaranteed and entrenched; the balanced_coexistence_reading interprets the framework as requiring ongoing negotiation between sovereignty and autonomy rather than legal supremacy. These are NOT different observations of the same constraint — they are different constraints produced by different readings of the same kernel text. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. The network links establish that the readings are interpretations of a common kernel and that changes to one reading's instantiation (enforcement intensity, prosecutorial scope, judicial carve-out breadth) structurally affect the viability of sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__sovereignty_primacy_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
