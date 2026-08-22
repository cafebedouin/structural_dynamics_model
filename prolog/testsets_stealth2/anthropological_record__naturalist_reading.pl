% ============================================================================
% CONSTRAINT STORY: anthropological_record__naturalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__naturalist_reading, []).

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
 *   constraint_id: anthropological_record__naturalist_reading
 *   human_readable: Naturalist Reading of the Anthropological Record: Credential-Governed Interpretive Regime
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   The anthropological record — fossils, strata, genomes, sites — is a
 *   contested kernel read differently by creationist,
 *   indigenous-epistemology, and naturalist communities. This file
 *   instantiates the naturalist reading as it actually operates
 *   institutionally: a credential-governed interpretive regime in which
 *   doctorate-holding specialists excavate, date, publish, and teach the
 *   record, while non-credentialed interpreters are heard at the regime's
 *   margins. The epsilon referent is the standing arrangement under contest —
 *   this credentialing regime itself, assessed by the reading's own lights —
 *   never the reading's endorsed alternative and never the bare empirical
 *   proposition, which is decomposed to its own story. Claim and metrics are
 *   authored independently: the claimed type states what this story takes the
 *   regime's structure to be; the metrics describe how it operates; the
 *   engine computes per-seat classifications from the structural data and
 *   owns any divergence.
 *
 * KEY AGENTS:
 *   - professional_anthropological_associations: agenda-setter (institutional/arbitrage) — convenes the field, publishes its flagships, writes its ethics codes
 *   - research_funding_agencies: agenda-setter with beneficiary position (institutional/arbitrage) — sets grant priorities through credentialed panels
 *   - credentialed_academic_anthropologists: primary beneficiary (powerful/constrained) — holds the licensed interpretive role while bearing the training-and-review treadmill
 *   - research_universities: beneficiary (institutional/mobile) — houses the record, collects tuition and indirect costs
 *   - scientific_publishers: beneficiary (powerful/arbitrage) — sells access to publicly funded findings
 *   - indigenous_knowledge_holders: primary payer (organized/identity_locked) — oral-tradition authority long set aside; ancestral remains held in institutional collections
 *   - creationist_communities: payer (organized/identity_locked) — courtroom-removed from public classrooms; maintains parallel institutions
 *   - independent_researchers: payer (powerless/constrained) — make surface discoveries, lack journal and grant access
 *   - students_and_taxpayers: beneficiary with payer position (moderate/mobile) — fund the pipeline, receive its packaged outputs
 *   - philosophy_of_science_observers: analytical observer — studies demarcation and credentialing from outside the hiring loop
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__naturalist_reading, 0.66).
domain_priors:suppression_score(anthropological_record__naturalist_reading, 0.48).
domain_priors:theater_ratio(anthropological_record__naturalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(anthropological_record__naturalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__naturalist_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__naturalist_reading, "Naturalist Reading of the Anthropological Record: Credential-Governed Interpretive Regime").
narrative_ontology:topic_domain(anthropological_record__naturalist_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__naturalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__naturalist_reading, '39b88983-25f6-46dc-bc51-a4ff4f73ac24').
narrative_ontology:cs_kernel_codification('39b88983-25f6-46dc-bc51-a4ff4f73ac24', distributed).
narrative_ontology:cs_authority_grounding('39b88983-25f6-46dc-bc51-a4ff4f73ac24', expertise).
narrative_ontology:cs_interpretation_layer_present('39b88983-25f6-46dc-bc51-a4ff4f73ac24').
narrative_ontology:cs_reading_relation('39b88983-25f6-46dc-bc51-a4ff4f73ac24', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_reading_relation('39b88983-25f6-46dc-bc51-a4ff4f73ac24', anthropological_record__indigenous_epistemology_reading, influences).
narrative_ontology:cs_axiom('39b88983-25f6-46dc-bc51-a4ff4f73ac24', foundational, material_causation_exhaustively_explanatory).
narrative_ontology:cs_axiom_status(material_causation_exhaustively_explanatory, holdable).
narrative_ontology:cs_axiom_grounding('39b88983-25f6-46dc-bc51-a4ff4f73ac24', material_causation_exhaustively_explanatory, empirically_contingent).
narrative_ontology:cs_axiom('39b88983-25f6-46dc-bc51-a4ff4f73ac24', foundational, epistemic_standing_requires_demonstrated_method_competence).
narrative_ontology:cs_axiom_status(epistemic_standing_requires_demonstrated_method_competence, holdable).
narrative_ontology:cs_axiom_grounding('39b88983-25f6-46dc-bc51-a4ff4f73ac24', epistemic_standing_requires_demonstrated_method_competence, instrumental).
narrative_ontology:cs_reference_frame('39b88983-25f6-46dc-bc51-a4ff4f73ac24', methodological_naturalist_consensus).
narrative_ontology:cs_drift_state('39b88983-25f6-46dc-bc51-a4ff4f73ac24', contemporary_repatriation_and_open_access_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('39b88983-25f6-46dc-bc51-a4ff4f73ac24', '').
narrative_ontology:cs_kernel_id(anthropological_record__naturalist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, credentialed_academic_anthropologists).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, research_universities).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, scientific_publishers).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, indigenous_knowledge_holders).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, creationist_communities).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, independent_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, research_funding_agencies).
narrative_ontology:constraint_beneficiary(anthropological_record__naturalist_reading, students_and_taxpayers).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, credentialed_academic_anthropologists).
narrative_ontology:constraint_victim(anthropological_record__naturalist_reading, students_and_taxpayers).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, common_descent_doctrine).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, out_of_africa_migration_model).
narrative_ontology:constraint_vindicates(anthropological_record__naturalist_reading, methodological_naturalism_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene the annual meetings where findings debut, publish the flagship journals, write the ethics codes governing fieldwork and curation, and administer the awards and committee slots that allocate standing within the discipline. They determine which questions count as settled and which interpreters count as qualified. Individual officers rotate; the association outlasts every member and faces little competitive pressure from rival disciplinary bodies.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, professional_anthropological_associations, agenda_setter,
    institutional, generational, arbitrage, global).

% Set the grant priorities that determine which origin questions get staffed and which lapse, using review panels drawn from the credentialed pool. Agency budgets scale with the research enterprise they fund, and panel service confers influence over the field's direction. Statutory mandates and political oversight bound their discretion.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, research_funding_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, research_funding_agencies, beneficiary).

% Hold the doctorates that license interpretation of the record: they excavate, date, publish, teach, and testify. Salaries, laboratory space, and speaking invitations flow through the credential they carry. The same pipeline charges them heavily — a decade of underpaid training, continuous review cycles, and grant-chasing that consumes much of a working life; leaving the academy forfeits both the premium and the accumulated investment.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, credentialed_academic_anthropologists, beneficiary,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, credentialed_academic_anthropologists, payer).

% Own the departments, museums, and collections where the record is housed and read. They collect tuition from students seeking the credential, indirect-cost recoveries on grants, and prestige rankings that drive donations. Endowments are diversified; a department can be opened or closed with a dean's signature.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, research_universities, beneficiary,
    institutional, generational, mobile, global).

% Operate the journals through which findings become citable, charging subscriptions and article fees for work funded largely by public money. Editorial boards are drawn from the credentialed pool, and rejection at a top journal can stall a career. Publishers can shift portfolios across disciplines and jurisdictions freely.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, scientific_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Hold sustained oral traditions naming ancestors, migrations, and relationships to specific places and remains. Their accounts were long set aside as legend while institutions excavated burial grounds and moved remains to distant museums; consultation now happens on terms set by the holding institutions. The traditions cannot be set aside without abandoning kinship obligations and place-based identity itself.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, indigenous_knowledge_holders, payer,
    organized, generational, identity_locked, regional).

% Maintain parallel schools, museums, and presses advancing readings of the record anchored in scriptural timelines and designed complexity. Courts have removed their account from public science classrooms, and their submissions rarely pass journal review; they respond by building separate institutions. The reading is load-bearing for their scriptural commitments; revising it would unsettle the community's shared identity.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, creationist_communities, payer,
    organized, generational, identity_locked, continental).

% Amateur fossil hunters, site stewards, and local historians frequently make the physical discoveries — surface finds, cave deposits, eroding riverbanks — but lack the degrees that unlock journal pages, grant lines, and curated collections. Their finds pass through credentialed intermediaries to be confirmed, credited, and catalogued; continuing without institutional affiliation means working unpublished and unfunded.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, independent_researchers, payer,
    powerless, biographical, constrained, national).

% Fund the apparatus through taxes and tuition and receive its outputs: curricula, museum exhibits, documentaries, and the trained specialists the system graduates. They choose among institutions and degree paths but encounter the interpretive framework only as it is packaged for them; dissenting households purchase alternatives on the private market.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, students_and_taxpayers, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(anthropological_record__naturalist_reading, students_and_taxpayers, payer).

% Study the demarcation problem — what separates reliable from unreliable knowledge practices — and the sociology of credentialing, from outside any laboratory's hiring loop. They publish critiques of peer review, testimony standards, and epistemic authority; their analyses change nothing directly but supply the vocabulary in which the other seats argue.
narrative_ontology:constraint_stakeholder(anthropological_record__naturalist_reading, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__naturalist_reading, credentialed_academic_anthropologists).
narrative_ontology:fixing_cost_class(anthropological_record__naturalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of a fragmentary, deep-time evidence corpus: shared stratigraphic, chronological, and genomic methods; replicated dating; trained readers; peer validation; and cumulative correction let dispersed researchers extend one another's findings instead of relitigating foundations each generation.
% TRANSFER_FUNCTION: Moves interpretive authority, research funding, publication access, and custody of the physical record toward credentialed institutions; moves tuition and tax revenue into the credentialing pipeline; and historically moved ancestral remains and cultural items from descendant communities into institutional collections, with partial return underway.
% ABSENT_VOICES: Non-credentialed interpreters enter the conversation only as subjects or rebuttal targets: creationist communities appear as survey respondents and litigants, indigenous knowledge holders as consultants on remains already held, independent researchers as correspondents. None sits on the editorial boards, hiring committees, or standards bodies where the interpretive framework is set. Present, they would contest the identification of credential with epistemic standing and the filing of oral tradition as data-at-best.
% DISAPPEARANCE_RATIONALE: Published findings would persist on library shelves, but the production, validation, and transmission machinery — journals, departments, museums, curricula, grant pipelines — would need wholesale reconstruction. Custody disputes over ancestral remains would reopen, the credential premium would evaporate overnight, and a successor gate would form around whatever filtering mechanism emerged next.
% FOUNDING_PROBLEM: After deep-time geology and Darwinian selection broke scriptural chronology, the founding problem was building a reliable, cumulative method for reading a fragmentary record — and deciding who was entitled to read it — against entrenched rival authorities: scriptural institutes, gentleman-amateur traditions, and living oral traditions dismissed as legend.
% FOUNDING_PROBLEM_CORROBORATION: Court records (Scopes 1925, McLean 1982, Kitzmiller 2005) corroborate that the founding collision was live and that the naturalist settlement was secured through public law, not internal consensus alone. Historians of science document the professionalization struggle from outside the benefiting parties. Indigenous testimony before domestic and international forums corroborates the exclusion half of the genealogy. Creationist organizations dispute the settlement but corroborate the collision itself. No party outside the credentialed complex attests that the founding problem is simply finished.
narrative_ontology:disappearance_verdict(anthropological_record__naturalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__naturalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__naturalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__naturalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__naturalist_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__naturalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__naturalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__naturalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is set at 0.66 because the regime's costs are real and concentrated: custody of the physical record, journal access sold back to the public that funded it, interpretive authority reserved to degree-holders, and a century of remains and items moved out of descendant communities — offset by a coordination function that genuinely works, which caps extraction below snare levels. Suppression is 0.48: overt coercive enforcement (litigation, statute, classroom exclusion) has receded since the education-law settlements, leaving routine gatekeeping as the residual force; alternatives persist in parallel institutions, so alternatives are narrowed, not erased. Theater is 0.28: peer review and replication do real work, but a growing minority of activity is metric ritual — impact chasing, citation games, audit compliance. Accessibility collapse is 0.45: inside the apparatus alternatives collapse almost completely; outside it, creationist and indigenous readings remain fully accessible, so the collapse is seat-relative and moderate overall. Resistance is 0.55: organized creationist movements, indigenous sovereignty campaigns, and open-access activism actively contest the regime. The temporal series run on one shared grid (nine points, all three metrics at every point). Base extractiveness traces a professionalization hump: rents accumulate as the credential monopoly consolidates through the grant-boom era, ease with repatriation law and open-access pressure, then tick back up with paywall consolidation and precarious labor. Suppression_requirement is authored deliberately: this story's dynamic IS enforcement-capacity change — courtroom machinery built up through the culture-war decades, peaked, then decayed after legal settlement made overt enforcement unnecessary — so the scalar suppression reflects the post-settlement steady state rather than the peak. Theater rises with metric culture and partially recedes as reform pressure registers.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (associations, funding agencies) should compute the arrangement as ordinary disciplinary self-governance; the payer seats should compute it as a closed guild. The insider academic seat straddles: beneficiaries of the very gate they staff, while bearing the publish-or-perish treadmill — a genuinely dual position the engine reads from the secondary role. Identity-lock binds the two principal payer seats by different mechanisms: for creationist communities the fusion is ideological (a scriptural worldview in which revising the reading unravels the faith community's shared identity), for indigenous knowledge holders it is relational (the epistemology is constituted by kinship and place, so exit means ceasing to be who they are). If either frame broke — a scriptural revision accepting deep time, or a community transferring custodial claims — those seats' effective extraction would drop sharply, and the classification would follow the structural data rather than this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the credentialed complex toward the beneficiary end: universities, publishers, and associations collect without bearing the gate's costs, with mobile or arbitrage exits placing them nearest the subsidy end. Credentialed academics derive low-but-not-zero directionality — they collect the credential premium yet pay the treadmill, and their constrained exit keeps them off the pure-beneficiary pole. Victim declarations drive the payer seats toward the target end, amplified by exit structure: identity-locked indigenous and creationist seats sit nearer the full-target end than mobile agents would, and powerless independent researchers with constrained exits sit close behind. Students and taxpayers derive near-symmetric directionality: genuine packaged knowledge received, tuition and taxes paid. The regime's global spatial scope modestly amplifies effective extraction for target seats because verification of fair treatment at scale is harder. Suppression enters the computation unscaled — it is a raw structural property; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a reliable method against rival authorities — is substantially accomplished inside the apparatus, yet the arrangement persists because the ongoing problem it now serves (validating new findings cumulatively) is real. The mandate transformed rather than died, which is why founding_problem_status is contested rather than dead: the dead-plus-world_rearranges mismatch that flags zombie capture does not fire, and the arrangement is not misread as theatrical residue. The tangled_rope claim guards against both misclassifications: reading the regime as pure extraction erases the genuine epistemic coordination that makes origin knowledge cumulative and correctable; reading it as pure coordination hides the custody transfers and authority exclusions that fall on named seats. Theater at 0.28 signals partial ritualization — metric performance inside a functioning machine — well short of the atrophied-performance profile of a piton, and the concentrated capture (named gain_flow seat) distinguishes this from diffuse-cost inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Does the classification computed for this reading''s institutional form describe the anthropological_record kernel itself, or would the sibling readings instantiate structurally different constraints?',
    'Generate the creationist_reading and indigenous_epistemology_reading stories and compare computed per-seat types across the family.',
    'If the siblings compute as differently shaped constraints (e.g., one dominated by enforcement, another by inertia), the kernel''s contest is between differently structured arrangements rather than one arrangement viewed from angles, and cross-reading epsilon comparisons must be referent-matched seat by seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Whether this reading''s computed type travels to the kernel or is indexical to the reading.').

omega_variable(
    credential_validity_ambiguity,
    'Is the correlation between credentials and interpretive reliability causal (training confers skill) or positional (credentials merely ration access)?',
    'Blind outcome studies comparing credentialed and non-credentialed interpreters on fossil identification, site dating, and skeletal analysis against ground-truthed test sets.',
    'If positional, the gatekeeping is access rationing riding on a real coordination function and the computed type shifts toward the extractive pole; if causal, part of the measured cost is the price of quality assurance and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_validity_ambiguity, empirical, 'Causal versus positional basis of the credential-reliability link.').

omega_variable(
    proposition_regime_decomposition,
    'Is this story''s epsilon about the empirical proposition (common descent, migration models — which assessed alone would show negligible extraction) or about the credentialing regime that transmits it?',
    'Author the bare empirical claim as its own near-mountain story and verify its epsilon diverges sharply from this regime story; the referent here is fixed to the standing credential-governed arrangement.',
    'Confirms the decomposition: conflating proposition and regime would either launder the regime''s costs behind the proposition''s solidity or smear the proposition with the regime''s rents. The two files must remain linked but separately classified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proposition_regime_decomposition, conceptual, 'Referent split between the empirical claim and the institutional regime.').

omega_variable(
    indigenous_data_sovereignty_trajectory,
    'Will collaborative archaeology, repatriation, and co-authorship norms continue opening the regime to indigenous interpretive authority, or will institutional control re-harden?',
    'Track repatriation completion rates, indigenous co-authored publication shares, and museum access-policy revisions over coming decades.',
    'Continued opening lowers the directionality of the indigenous seat and could move the computed type toward the coordination pole; re-hardening raises effective extraction toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_data_sovereignty_trajectory, empirical, 'Direction of the regime''s opening to descendant-community authority.').

omega_variable(
    suppression_decay_measurement_artifact,
    'Is the falling suppression_requirement series genuine enforcement decay (the regime now self-sustains through default authority) or a measurement artifact (overt courtroom enforcement replaced by subtler gatekeeping in review, hiring, and funding)?',
    'Normalize manuscript rejection rates, grant-panel exclusion patterns, and hiring-filter effects across eras against applicant-pool composition.',
    'If subtle gatekeeping replaced overt enforcement, the scalar suppression understates the regime''s current coercive force and the computed extraction for trapped seats is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_decay_measurement_artifact, empirical, 'Whether declining overt enforcement reflects decay or displacement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__naturalist_reading, 0, 160).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__naturalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__naturalist_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(anth_tr_t20, observed).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__naturalist_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement_basis(anth_tr_t40, observed).
narrative_ontology:measurement(anth_tr_t60, anthropological_record__naturalist_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement_basis(anth_tr_t60, observed).
narrative_ontology:measurement(anth_tr_t80, anthropological_record__naturalist_reading, theater_ratio, 80, 0.23).
narrative_ontology:measurement_basis(anth_tr_t80, observed).
narrative_ontology:measurement(anth_tr_t100, anthropological_record__naturalist_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(anth_tr_t100, observed).
narrative_ontology:measurement(anth_tr_t120, anthropological_record__naturalist_reading, theater_ratio, 120, 0.32).
narrative_ontology:measurement_basis(anth_tr_t120, observed).
narrative_ontology:measurement(anth_tr_t140, anthropological_record__naturalist_reading, theater_ratio, 140, 0.31).
narrative_ontology:measurement_basis(anth_tr_t140, observed).
narrative_ontology:measurement(anth_tr_t160, anthropological_record__naturalist_reading, theater_ratio, 160, 0.28).
narrative_ontology:measurement_basis(anth_tr_t160, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__naturalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t20, anthropological_record__naturalist_reading, base_extractiveness, 20, 0.41).
narrative_ontology:measurement_basis(anth_be_t20, observed).
narrative_ontology:measurement(anth_be_t40, anthropological_record__naturalist_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement_basis(anth_be_t40, observed).
narrative_ontology:measurement(anth_be_t60, anthropological_record__naturalist_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(anth_be_t60, observed).
narrative_ontology:measurement(anth_be_t80, anthropological_record__naturalist_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement_basis(anth_be_t80, observed).
narrative_ontology:measurement(anth_be_t100, anthropological_record__naturalist_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement_basis(anth_be_t100, observed).
narrative_ontology:measurement(anth_be_t120, anthropological_record__naturalist_reading, base_extractiveness, 120, 0.69).
narrative_ontology:measurement_basis(anth_be_t120, observed).
narrative_ontology:measurement(anth_be_t140, anthropological_record__naturalist_reading, base_extractiveness, 140, 0.64).
narrative_ontology:measurement_basis(anth_be_t140, observed).
narrative_ontology:measurement(anth_be_t160, anthropological_record__naturalist_reading, base_extractiveness, 160, 0.66).
narrative_ontology:measurement_basis(anth_be_t160, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__naturalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t20, anthropological_record__naturalist_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement_basis(anth_su_t20, observed).
narrative_ontology:measurement(anth_su_t40, anthropological_record__naturalist_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement_basis(anth_su_t40, observed).
narrative_ontology:measurement(anth_su_t60, anthropological_record__naturalist_reading, suppression_requirement, 60, 0.47).
narrative_ontology:measurement_basis(anth_su_t60, observed).
narrative_ontology:measurement(anth_su_t80, anthropological_record__naturalist_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement_basis(anth_su_t80, observed).
narrative_ontology:measurement(anth_su_t100, anthropological_record__naturalist_reading, suppression_requirement, 100, 0.57).
narrative_ontology:measurement_basis(anth_su_t100, observed).
narrative_ontology:measurement(anth_su_t120, anthropological_record__naturalist_reading, suppression_requirement, 120, 0.61).
narrative_ontology:measurement_basis(anth_su_t120, observed).
narrative_ontology:measurement(anth_su_t140, anthropological_record__naturalist_reading, suppression_requirement, 140, 0.58).
narrative_ontology:measurement_basis(anth_su_t140, observed).
narrative_ontology:measurement(anth_su_t160, anthropological_record__naturalist_reading, suppression_requirement, 160, 0.48).
narrative_ontology:measurement_basis(anth_su_t160, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__naturalist_reading, information_standard).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, anthropological_record__indigenous_epistemology_reading).
narrative_ontology:affects_constraint(anthropological_record__naturalist_reading, common_descent_empirical_claim).

% DUAL FORMULATION NOTE:
% The colloquial label 'what the anthropological record reveals' decomposes into at least three structurally distinct constraints, one per declared reading of the kernel. This file authors the naturalist reading only: its epsilon (0.66) measures the credential-governed interpretive regime as this reading's institutional form stands — not the truth of common descent, which is a separate near-zero-extraction empirical question warranting its own story (linked here as common_descent_empirical_claim), and not the sibling readings' arrangements. Family members link via affects_constraints. Downstream structure: the naturalist regime's institutional success sets the legitimacy conditions under which the sibling readings are marginalized or accommodated, which is why this reading carries an influences edge to the indigenous reading and a foreclosure edge to the creationist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
