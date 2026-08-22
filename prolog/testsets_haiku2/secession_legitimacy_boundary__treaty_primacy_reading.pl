% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__treaty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__treaty_primacy_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__treaty_primacy_reading
 *   human_readable: Indigenous Treaty Primacy Over Secession Legitimacy
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   The treaty primacy reading holds that Indigenous treaty rights—recognized
 *   in international law, constitutional orders, and historical
 *   agreements—predate and supersede both federal and provincial authority.
 *   Under this reading, no secession is legitimate without the consent of
 *   Indigenous treaty holders whose territories would be affected. The
 *   constraint operates as a veto gate on provincial independence movements:
 *   a region cannot unilaterally exit the federation if doing so would alter
 *   Indigenous territorial claims or legal status without Indigenous consent.
 *   This reading competes with three sibling readings:
 *   constitutional_impossibility_reading (unilateral secession is always
 *   constitutionally forbidden, regardless of treaty status),
 *   popular_sovereignty_reading (provincial democratic majorities hold
 *   ultimate sovereignty), and grievance_threshold_reading (secession becomes
 *   legitimate when federal injustice crosses a threshold). The treaty
 *   primacy reading is CLAIMED as tangled_rope because it solves a genuine
 *   coordination problem (preventing territorial chaos from unilateral
 *   redrawing) while imposing asymmetric costs on provincial secessionists
 *   who lose unilateral exit authority. The authored metrics describe
 *   moderately extractive operation with rising enforcement intensity over
 *   the interval.
 *
 * KEY AGENTS:
 *   - Indigenous treaty holders: territorial authority and veto over secession legitimacy, identity-locked into treaty framework
 *   - Secession-seeking provincial majority: constrained exit, must negotiate with Indigenous nations, bears cost of delayed/forbidden independence
 *   - Federal government: administers the constraint but cannot escape it; treaty obligation supersedes its own authority
 *   - Provincial government: both potential separatist and target of the constraint; subordinate to Indigenous treaty rights
 *   - International legal community: observer seat; evaluates secession legitimacy against treaty and Indigenous rights principles
 *   - Non-Indigenous provincial residents: excluded from consent authority; their democratic preference is insufficient
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, 0.68).
domain_priors:suppression_score(secession_legitimacy_boundary__treaty_primacy_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__treaty_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__treaty_primacy_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__treaty_primacy_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__treaty_primacy_reading, "Indigenous Treaty Primacy Over Secession Legitimacy").
narrative_ontology:topic_domain(secession_legitimacy_boundary__treaty_primacy_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__treaty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__treaty_primacy_reading, 'f6155928-a5ed-4a80-9119-e8193850fdeb').
narrative_ontology:cs_kernel_codification('f6155928-a5ed-4a80-9119-e8193850fdeb', fixed_text).
narrative_ontology:cs_authority_grounding('f6155928-a5ed-4a80-9119-e8193850fdeb', lineage).
narrative_ontology:cs_interpretation_layer_present('f6155928-a5ed-4a80-9119-e8193850fdeb').
narrative_ontology:cs_reading_relation('f6155928-a5ed-4a80-9119-e8193850fdeb', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6155928-a5ed-4a80-9119-e8193850fdeb', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('f6155928-a5ed-4a80-9119-e8193850fdeb', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('f6155928-a5ed-4a80-9119-e8193850fdeb', foundational, indigenous_treaty_rights_predate_state).
narrative_ontology:cs_axiom_status(indigenous_treaty_rights_predate_state, holdable).
narrative_ontology:cs_axiom_grounding('f6155928-a5ed-4a80-9119-e8193850fdeb', indigenous_treaty_rights_predate_state, deontological).
narrative_ontology:cs_axiom('f6155928-a5ed-4a80-9119-e8193850fdeb', foundational, territorial_legitimacy_requires_indigenous_consent).
narrative_ontology:cs_axiom_status(territorial_legitimacy_requires_indigenous_consent, holdable).
narrative_ontology:cs_axiom_grounding('f6155928-a5ed-4a80-9119-e8193850fdeb', territorial_legitimacy_requires_indigenous_consent, deontological).
narrative_ontology:cs_reference_frame('f6155928-a5ed-4a80-9119-e8193850fdeb', pre_colonial_indigenous_sovereignty).
narrative_ontology:cs_drift_state('f6155928-a5ed-4a80-9119-e8193850fdeb', contemporary_post_colonial_secession_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f6155928-a5ed-4a80-9119-e8193850fdeb', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, secession_seeking_provincial_majority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, regional_non_indigenous_population).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess treaty rights recognized in international law and domestic constitutional orders that predate the federal and provincial state structures. These rights confer authority to govern their territories, manage resources, and consent to territorial changes that affect their lands. The treaty primacy reading grants them veto power over secession that would alter their legal status or territorial claims. Their identity as treaty peoples and their historical connection to territory make exit from the framework structurally impossible — they cannot renounce being Indigenous or the territorial claims that flow from historic occupancy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders, beneficiary,
    moderate, generational, identity_locked, national).

% A provincial electorate seeking to separate from the federation, mobilizing democratic majorities for independence. Under the treaty primacy reading, their preferred exit path is blocked until Indigenous treaty consent is obtained. They bear the cost of delayed or forbidden secession, and must negotiate with Indigenous nations who hold legal claim to overlapping or adjacent territory. Their alternative is to proceed without Indigenous consent (risking international delegitimization and territorial disputes) or abandon the secession project entirely.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, secession_seeking_provincial_majority, payer,
    organized, biographical, constrained, national).

% Holds formal constitutional authority but, under the treaty primacy reading, cannot unilaterally determine secession legitimacy because treaty rights supersede federal authority itself. The federal government is bound by the same treaty constraints it cannot dissolve. It administers the federal-provincial structure and can invoke the treaty primacy rule to block or delay secession, or it can mediate between provincial separatists and Indigenous nations. Its enforcement leverage comes from treaty obligation itself.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Holds territorial jurisdiction but cannot unilaterally determine secession legitimacy under the treaty primacy reading. Provincial authority is subordinate to pre-existing Indigenous treaty rights. The province is both a potential secession-seeking agent (if its electorate votes to leave) and a target of the constraint (its authority is limited by treaty). It must negotiate with Indigenous treaty holders or proceed with international legal vulnerability.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__treaty_primacy_reading, provincial_government, agenda_setter).

% Evaluates secession claims against principles of self-determination, territorial integrity, and Indigenous rights. Under the treaty primacy reading, international law recognizes Indigenous self-determination as a distinct entitlement that constrains unilateral provincial secession. International bodies (UN mechanisms, International Court of Justice, regional human rights courts) observe and adjudicate whether secession proceeded with Indigenous consent, and may refuse to recognize breakaway states that violated treaty-holder rights.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, international_legal_community, observer,
    institutional, generational, analytical, universal).

% Represent the provincial majority seeking secession but lack standing to consent on behalf of the territory because they do not hold Indigenous treaty claims to the land. The treaty primacy reading structurally excludes them from the consent authority, treating their democratic preference as insufficient without Indigenous agreement. They can leave the province, accept the secession delay, negotiate with Indigenous nations, or repudiate the treaty primacy principle entirely (shifting to a different reading).
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, non_indigenous_provincial_residents, excluded,
    organized, biographical, mobile, national).

% Interpret the constitutional status of treaties, federalism, and secession. They adjudicate whether treaty rights legally supersede secession claims. Different constitutional courts (Canadian Supreme Court, other federal judiciaries) may reach different verdicts on the treaty primacy principle itself.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__treaty_primacy_reading, constitutional_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_treaty_holders).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__treaty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes territorial authority and resource governance by grounding sovereignty claims in pre-existing, internationally recognized Indigenous treaty relationships rather than in contemporary democratic majorities alone. Prevents unilateral territorial rearrangement that would alter the legal status of Indigenous nations without their consent.
% TRANSFER_FUNCTION: Transfers veto power over territorial legitimacy from provincial/federal governments to Indigenous treaty holders. Provincial separatists must surrender their ability to exit unilaterally and instead negotiate with Indigenous nations to gain legitimacy. Resource rights tied to seceding territory flow through treaty consultation requirements.
% ABSENT_VOICES: Non-Indigenous regional residents who support secession are structurally excluded from the consent authority under this reading — their voice is heard in provincial referenda but does not carry veto weight over territorial disposition. They would argue that democratic majority preference should determine territorial fate, but are kept out of the consent authority by the treaty primacy principle itself. Separatist intellectuals and independence movements would dispute the constraint altogether.
% DISAPPEARANCE_RATIONALE: If treaty primacy over secession legitimacy disappeared overnight, provincial majorities could proceed to unilateral independence without Indigenous consent. Territorial disputes would proliferate — Indigenous nations would claim overlapping territories, refuse recognition of breakaway states, and seek international intervention. Resource extraction in disputed zones would accelerate conflict. Multiple competing sovereignties would emerge where a single federal structure once held. The International Court of Justice and Indigenous rights bodies would face cascading contested legitimacy claims.
% FOUNDING_PROBLEM: Early colonial and post-colonial states (particularly in settler-colonial jurisdictions like Canada, Australia) established federal structures that marginalized Indigenous territorial claims and self-determination rights. Treaties were signed recognizing Indigenous sovereignty but subsequently subordinated to federal/provincial jurisdictions. Secession movements in the late 20th and 21st centuries threatened to finalize this subordination by carving out new states without consulting the Indigenous nations whose territorial claims preceded the federal state itself.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies (UN Permanent Forum on Indigenous Issues, International Expert Mechanism on the Rights of Indigenous Peoples) attest the founding problem is live: Indigenous nations continue to assert territorial claims and self-determination rights in secession-affected regions. Canadian courts (in Haida Nation and similar decisions) have recognized treaty consultation requirements. Indigenous legal scholars and Indigenous nations themselves provide corroboration outside the beneficiary set — Tsimshian, Tlingit, Wet'suwet'en, and other Pacific Northwest nations have asserted veto authority over territorial changes affecting their territories, independently of whether they 'benefit' in the conventional sense.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__treaty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__treaty_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__treaty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__treaty_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__treaty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__treaty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness of 0.68 reflects the substantial cost imposed on provincial majorities who lose unilateral exit authority. The constraint transfers veto power from democratic processes to treaty-rights holders, a clear asymmetric allocation. Suppression of 0.71 is high because maintaining the treaty primacy principle requires active enforcement: courts must enjoin provincial governments from proceeding without Indigenous consent; international bodies must refuse recognition to breakaway states that violate treaty obligations; federal enforcement must be mobilized to prevent unilateral secession. Theater ratio of 0.41 indicates genuine coordination content (preventing territorial chaos) but also performative elements (invocations of treaty rights in rhetoric vs. actual consultation processes). The measurement series shows rising extractiveness and suppression over the 40-year interval, reflecting increasing Indigenous mobilization and international recognition of their claims — early periods showed weaker enforcement, more successful unilateral provincial moves; later periods show tighter constraints. This rising trajectory is empirically observed in Canadian jurisprudence (Haida Nation 2004 onwards) and international law evolution. The shared time grid aligns all three metrics at every measurement point.
 *
 * PERSPECTIVAL GAP:
 *   The Indigenous treaty-holder seat and the secession-seeking provincial seat should compute radically different types. From the Indigenous position, the arrangement is coordination: preventing territorial chaos and securing territorial sovereignty. From the provincial separatist seat, it is extraction: losing exit authority to a veto holder. The federal government's seat is Janus-faced: the federal government gains legitimacy from treaty-grounding but loses flexibility. International observers compute neither benefit nor cost — they are neutral adjudicators. The engine computes per-seat classifications and should show this divergence clearly: Indigenous seat → rope/coordination, provincial separatist seat → snare/extraction. The disparity in computed types is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty holders derive d near 0.0 (full beneficiary): they gain veto authority, territorial protection, and recognition of sovereignty claims without bearing the cost of enforcement (federal/international bodies enforce the constraint on their behalf). Secession-seeking provincial majorities derive d near 1.0 (full target): they lose unilateral exit, must negotiate, face delayed independence, and bear the organizational cost of treaty consultation. Federal government sits near d=0.5 (symmetric): it gains legitimacy from treaty-grounded authority but loses flexibility to unilaterally permit secession; treaty obligation constrains federal sovereignty itself. Non-Indigenous provincial residents are excluded from the directionality calculation altogether — their democratic preference is not the seat from which d is computed; they are organized into 'secession_seeking_provincial_majority' and inherit its high-target directionality. The constraint's effectiveness depends on federal and international enforcement of Indigenous veto power, so those institutions are the structural seats that matter for d computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial subordination of Indigenous rights, unfinished decolonization) is alive and contested. Secession movements in the 21st century directly threaten to finalize the colonial erasure by creating new states without Indigenous consent. The treaty primacy reading prevents mandatrophy by grounding secession legitimacy in pre-existing Indigenous authority rather than in the federal/provincial structure. However, there is a secondary mandatrophy risk: if Indigenous nations use veto authority to extract concessions from secession movements (money, resource rights, political seats) rather than to assert genuine territorial self-determination, the constraint risks becoming a pure extortion mechanism. The omega variable 'veto_capture_risk' addresses this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_consent_definition_ambiguity,
    'What counts as Indigenous treaty holder ''consent'' to secession? Is it unanimous consent from all nations with territorial claims, majority-vote consent from treaty-holder representatives, or consent from the primary territorial nation?',
    'Legal and political negotiation between Indigenous nations and provincial/federal governments. International Indigenous rights bodies (UN EMRIP) could provide guidance on standard consent protocols.',
    'A narrow consent definition (unanimous, or only primary nation) makes secession nearly impossible; a broad consent definition (any treaty holder plus payment) reduces the constraint''s practical force. Different consent thresholds produce different effective extractiveness values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_consent_definition_ambiguity, conceptual, 'Whether treaty holder veto is univocal or aggregated across multiple nations with overlapping claims.').

omega_variable(
    treaty_primacy_vs_constitutional_authority,
    'Do treaty rights truly supersede federal and provincial constitutional authority, or are they subordinate to the constitutional order that recognizes them?',
    'Constitutional court decisions (Supreme Court of Canada in landmark cases; international court opinions; comparative constitutional law analysis across settler-colonial jurisdictions).',
    'If treaties are supreme, the constraint is structurally binding and secession is blocked. If treaties are subordinate to constitutional authority, the federal government can unilaterally amend the constitution to override Indigenous consent requirements, reducing the constraint to theater. This is the core empirical question the entire kernel contest turns on.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_primacy_vs_constitutional_authority, empirical, 'The canonical jurisdictional ranking: are treaties truly foundational or merely constitutionally-grounded?').

omega_variable(
    veto_capture_risk,
    'Could Indigenous nations use treaty veto authority to extract political and economic concessions from secession movements in ways that instrumentalize their consent rather than serving genuine territorial self-determination?',
    'Observation of actual negotiation outcomes in secession scenarios where Indigenous veto authority is exercised. Qualitative analysis of whether extracted concessions serve Indigenous sovereignty or become payment for consent that Indigenous nations would otherwise withhold.',
    'If veto authority is captured by Indigenous leadership for private gain rather than nation-building, the constraint''s mandatrophy risk rises sharply: the arrangement would become a tool for extracting resources from provincial majorities without advancing Indigenous self-determination. This does not change the constraint''s classification but signals institutional corruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_capture_risk, empirical, 'Whether Indigenous veto is exercised for territorial self-determination or for resource extraction.').

omega_variable(
    kernel_reading_plurality,
    'Which of the four readings of the secession_legitimacy_boundary kernel — constitutional_impossibility, popular_sovereignty, grievance_threshold, or treaty_primacy — correctly characterizes legitimate territorial authority?',
    'This is a conceptual/preference question at the kernel level, not resolvable by empirical data alone. Different readings are held by different constitutional traditions, Indigenous nations, and political movements. No single reading can be declared ''correct'' without choosing among competing foundational premises about sovereignty.',
    'This omega documents the irresolvable plurality at the kernel level. This JSON instantiates only the treaty_primacy reading and treats it as self-contained. Sibling readings are other constraint stories in the corpus, not alternative interpretations within this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_plurality, conceptual, 'The kernel-level reading plurality: no single reading is universally correct; each reading articulates a coherent but contested principle of legitimate authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__treaty_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(sece_tr_t8, observed).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(sece_tr_t16, observed).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(sece_tr_t24, observed).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(sece_tr_t32, projected).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__treaty_primacy_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(sece_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(sece_be_t8, observed).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(sece_be_t16, observed).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement_basis(sece_be_t24, observed).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(sece_be_t32, projected).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__treaty_primacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(sece_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(sece_su_t8, observed).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(sece_su_t16, observed).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(sece_su_t24, observed).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(sece_su_t32, projected).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__treaty_primacy_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(sece_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__treaty_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__treaty_primacy_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, indigenous_land_claim_recognition__territorial_boundaries).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__treaty_primacy_reading, federal_indigenous_consultation_requirement__resource_extraction).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested secession_legitimacy_boundary kernel. The kernel is read as four structurally distinct constraints: treaty_primacy_reading (this one), constitutional_impossibility_reading, popular_sovereignty_reading, and grievance_threshold_reading. Each reading instantiates different beneficiaries, victims, and enforcement mechanisms. They share the same referent (the territorial legitimacy question) but differ in which principle grounds legitimate authority. Treaty primacy is substantially more extractive than constitutional_impossibility (which treats secession as categorically forbidden) and more asymmetric than popular_sovereignty (which treats democratic majorities as self-legitimating). The ε-invariance test confirms these are distinct constraints: measuring the treaty primacy constraint by a different observable (federal constitutional text instead of treaty rights) yields a different constraint altogether.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secession_legitimacy_boundary__treaty_primacy_reading, organized, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
