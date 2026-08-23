% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Oral-Tradition Epistemic Authority over the Ancestral Record (Indigenous Epistemology Reading)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This file authors ONE reading of the contested kernel
 *   anthropological_record, per the epsilon-invariance principle. The kernel
 *   label 'the record' colloquially conflates three structurally distinct
 *   claims: what the material trace of the human past reveals, and how it may
 *   be known. The naturalist sibling (constraint
 *   anthropological_record__naturalist_reading) claims the record reveals
 *   materialist origins knowable via scientific method; the creationist
 *   sibling (constraint anthropological_record__creationist_reading) claims
 *   it reveals divine creation events compatible with scriptural
 *   transmission; THIS story instantiates the
 *   indigenous_epistemology_reading: the record reveals relational continuity
 *   with ancestors and place, knowable via sustained oral tradition, with
 *   community authority over ancestral remains and credentialed and
 *   scriptural frameworks subordinated to it. Each reading is a separate
 *   constraint with its own epsilon, beneficiaries, and victims; they are
 *   linked through network.affects_constraints, not merged. The epsilon
 *   referent here is the standing arrangement under contest as this reading
 *   sees it: the regime of oral-tradition-mediated access and community
 *   custody over ancestral remains. Assessed by this reading's lights, the
 *   arrangement is substantially protective coordination carrying real but
 *   largely legitimated costs for credentialed access-holders. KEY AGENTS (by
 *   structural relationship): descendant_communities
 *   (organized/identity_locked) — primary beneficiary, holds custody and
 *   approves research; traditional_knowledge_keepers
 *   (moderate/identity_locked) — beneficiary whose testimony carries formal
 *   standing; credentialed_archaeologists (organized/constrained) — primary
 *   payer among researchers; museum_collections_holders
 *   (institutional/constrained) — payer bearing return obligations;
 *   paleogenomics_laboratories (powerful/constrained) — payer gated at the
 *   sampling step; federal_heritage_agencies (institutional/constrained) —
 *   agenda_setter administering enforcement; avocational_collector_societies
 *   (moderate/mobile) — excluded voice; philosophy_of_science_observers
 *   (analytical) — analytical observer seeing the full three-reading
 *   structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.45).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Oral-Tradition Epistemic Authority over the Ancestral Record (Indigenous Epistemology Reading)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, 'e99931ff-f6d5-4e61-8ab2-6c913f28a5e7').
narrative_ontology:cs_kernel_codification('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', distributed).
narrative_ontology:cs_authority_grounding('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', lineage).
narrative_ontology:cs_interpretation_layer_present('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7').
narrative_ontology:cs_reading_relation('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', anthropological_record__naturalist_reading, influences).
narrative_ontology:cs_reading_relation('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', foundational, material_evidence_insufficient_without_oral_tradition).
narrative_ontology:cs_axiom_status(material_evidence_insufficient_without_oral_tradition, holdable).
narrative_ontology:cs_axiom_grounding('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', material_evidence_insufficient_without_oral_tradition, empirically_contingent).
narrative_ontology:cs_axiom('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', foundational, community_authority_over_ancestral_remains).
narrative_ontology:cs_axiom_status(community_authority_over_ancestral_remains, holdable).
narrative_ontology:cs_axiom_grounding('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', community_authority_over_ancestral_remains, deontological).
narrative_ontology:cs_reference_frame('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', living_relational_continuity).
narrative_ontology:cs_drift_state('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', contemporary_codification_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e99931ff-f6d5-4e61-8ab2-6c913f28a5e7', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, descendant_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, traditional_knowledge_keepers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, credentialed_archaeologists).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, museum_collections_holders).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, paleogenomics_laboratories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities descended from the peoples whose remains, burial sites, and named places are at issue. They receive formal decision rights over the disposition of ancestral remains, approve or decline research proposals, and supply the oral accounts through which the record is read. Leaving is not a meaningful option for them: the relationship to ancestors and place constitutes who they are, so exit would mean dissolving the community's own continuity.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, descendant_communities, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__indigenous_epistemology_reading, descendant_communities, agenda_setter).

% Elders and designated custodians who hold and transmit sustained oral accounts of migrations, place-names, burials, and events. Their testimony now carries formal weight in consultations and disposition determinations, and their standing rises as institutions must come to them rather than the reverse. Their role is inseparable from their identity and lifelong transmission duties.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, traditional_knowledge_keepers, beneficiary,
    moderate, generational, identity_locked, regional).

% University-affiliated excavators and analysts whose fieldwork proceeds only under permits contingent on community approval, and whose publications and disposition recommendations are subject to community sign-off. Careers built on particular regions and collections cannot be relocated with the material; their realistic options are collaboration on community terms, shifting to other questions or jurisdictions, or leaving the material untouched.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, credentialed_archaeologists, payer,
    organized, biographical, constrained, global).

% Museums and universities holding ancestral remains and cultural items acquired during the collecting era. They carry inventory, documentation, consultation, and return obligations that steadily empty portions of their holdings, reshape galleries and teaching collections, and redirect curatorial staff time. Compliance is legally mandated; refusal draws penalties and public censure.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, museum_collections_holders, payer,
    institutional, generational, constrained, continental).

% Government offices that administer the statutes governing treatment and return of Native American remains and items: they publish regulations, set compliance deadlines, run review committees, fund consultation, and penalize delinquent institutions. The agencies did not originate the arrangement and cannot unilaterally abandon it; their discretion lies in pacing and strictness of implementation.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, federal_heritage_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Ancient-DNA facilities whose methods consume irreplaceable skeletal material. Sampling contested remains requires community assent, and several whole research programs on human deep history now depend on access that is granted or withheld case by case. Laboratories can redirect instrumentation toward other species and periods, but the human-origins program itself sits behind the assent gate.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, paleogenomics_laboratories, payer,
    powerful, biographical, constrained, global).

% Amateur archaeology clubs and private collectors who once surfaced-collected on ancestral lands and are now barred from doing so. They would argue for licensed hobbyist access and against professional monopolies on the material, but they sit outside the consultation tables where protocols are drafted, and their objections surface mainly in comment periods and letters.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, avocational_collector_societies, excluded,
    moderate, biographical, mobile, national).

% Comparative epistemologists and historians of science who study how evidentiary authority over the deep human past is allocated among instruments, texts, and transmitted testimony. They collect testimony from the other seats, publish analyses, and bear neither the obligations nor the returns of the participants.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, philosophy_of_science_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, descendant_communities).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how descendant peoples, researchers, and agencies can handle ancestral remains, burial places, and site access so that knowledge production proceeds without desecration and incorporates knowledge that only transmitted oral accounts carry: which places are burial grounds, which remains belong to whom, what migration and event histories attach to landscapes.
% TRANSFER_FUNCTION: Moves decision rights over ancestral remains, burial sites, and research access from credentialed institutions and individual investigators to descendant communities, and moves epistemic standing from instrument-based measurement alone toward sustained oral testimony delivered under community authority.
% ABSENT_VOICES: Avocational collectors are structurally excluded from protocol-writing tables despite bearing access losses. Researchers in jurisdictions without comparable community-authority regimes benefit from arbitrage and have no seat here. Future generations of scientists cannot object to irreversible choices made now, including destructive sampling refusals and reburials without analysis. Within communities themselves, holders who resist translating ceremonial knowledge into bureaucratic consultation formats often go unheard because formal representation runs through recognized tribal offices.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, repatriation statutes, consultation mandates, museum compliance machinery, and permit conditions would all lose their basis: collections would reopen to unrestricted research, disposition decisions would revert to institutions, and the trust relations painstakingly built through decades of negotiation would collapse back into litigation and protest. Research access would expand; community custody and oral-testimony standing would evaporate.
% FOUNDING_PROBLEM: The collecting era: graves robbed for museum cabinets, craniometric race science performed on ancestors without consent, oral testimony dismissed as myth while skeletons were measured, and research published about descendant peoples with no participation from them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: congressional hearing records documenting the grave-looting and crania-trade economy that preceded the statutes, museum-professional-association statements acknowledging collection-era wrongdoing, court findings in early repatriation disputes, and independent scholarly histories of craniometry. Geological corroboration studies of dated oral accounts (eruption recollections, drowned coastlines) independently attest that transmitted testimony carries verifiable deep-time content, supporting the epistemic half of the founding grievance.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).
:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45 because the transfer is real and identifiable — decision rights over remains, permit gating, publication sign-off, permanent loss of collections and of destructive-analysis opportunities — but from this reading's seat much of that cost is legitimated as restitution for the collecting era and as proper deference to custodial duty, which tempers rather than eliminates the extraction. Suppression is 0.62 and is structural: statutory mandates, regulations, permit denial, and penalty schedules constrain the alternative of unconsented study; there is little coercive force beyond access denial, and per the framework suppression is authored as a raw unscaled property regardless of scope. Theater_ratio is 0.20: consultation occasionally degenerates into box-ticking, but the protective and transmission functions are genuinely performed, so performative share stays low. Accessibility_collapse is 0.55: inside regulated jurisdictions, once the rule is understood the alternative collapses to comply-or-abstain for contested materials, but workarounds persist (non-contested specimens, other countries' materials, non-human archives), so collapse is partial. Resistance is 0.50: decades of professional litigation, institutional delay, and editorial objection, gradually yielding to compliance. The temporal series run on ONE shared grid (1969, 1979, 1990, 1999, 2008, 2017, 2025) with every tracked metric authored at every point; the suppression_requirement series is authored deliberately because enforcement capacity is the dynamic this story traces — it matured from pre-statutory community pressure (0.22) through statute and regulation (0.42, 0.55) to hardened administrative compliance (0.62) — and its end state matches the base scalar. Extractiveness rises with enforcement bite and then eases slightly as collaborative offsets emerged; theater bumps with proceduralization and declines as joint work matured. No cyclical dynamics are claimed: trajectories are monotone with a late plateau, not oscillatory.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply divergent per-seat types from this structural data. From the descendant-community seat the arrangement presents as protective coordination it helped build and now stewards: low extraction, subsidy-flavored, rope-like. From the credentialed-researcher and laboratory seats the same structure operates as enforced transfer: their autonomy, collections, and sampling programs pay for it, and their constrained exits amplify effective extraction toward the target end. The agency seat sits administratively between, enforcing without originating. Identity-lock binds the beneficiary seats through RELATIONAL identity: self-concept constituted through continuity with ancestors and place, making exit conceptually unavailable rather than merely costly; were that identity frame to dissolve (complete assimilation severing the ancestor-place relation), the constraint would lose its maintaining constituency and decay toward inertial performance. Payer seats show weaker professional identity-lock: career path dependence constrains individual archaeologists, but laboratories and museums retain partial mobility, which is why their extraction amplifies less than a fully trapped target's would.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: descendant_communities and traditional_knowledge_keepers sit near the beneficiary pole (d near 0), receiving custody rights, evidentiary standing, and protection at little personal cost. Victim declarations place credentialed_archaeologists, museum_collections_holders, and paleogenomics_laboratories near the target pole (d near 1), with constrained exits keeping them from arbitrage relief. federal_heritage_agencies derive near-symmetric: they administer and enforce, funding consultation while absorbing compliance costs internally, collecting no rents of the kind the beneficiary seats collect. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already yield the correct qualitative placement for every seat, and an override keyed to a shared power atom would misapply across seats (for instance, an override on 'organized' would hit both descendant_communities and credentialed_archaeologists, whose directionalities are opposed). Scope amplification applies modestly at national scale where verification of compliance is administratively feasible.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — collecting-era desecration and the dismissal of transmitted testimony — remains live, corroborated from outside the beneficiary set, and the enforcement apparatus is still gaining capacity rather than decaying, so the arrangement has not outlived its function. Theater remains low, which independently argues against piton drift. The classification discipline matters here in both directions: reading the structure as pure rope would erase who pays — the subordination of credentialed and scriptural frameworks is a real transfer with identifiable cost-bearers, not overhead; reading it as pure snare would erase the genuine coordination function — protection of ancestors and integration of testimony that instruments cannot supply are real goods the arrangement delivers, not cover. The tangled-rope claim keeps both halves visible: coordinated access and custody on one face, enforced transfer of epistemic authority on the other. If the enforcement ratchet instead reversed — statutes repealed, compliance lapsing — the structure would decay toward scaffold-like transitoriness or piton-like theatrical residue, and the temporal series would date that turn.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_status,
    'This story instantiates only the indigenous_epistemology_reading of the kernel anthropological_record. Does its classification hold as a property of the kernel, or does it index strictly to this reading?',
    'Compile and compare the sibling stories (anthropological_record__naturalist_reading, anthropological_record__creationist_reading), which share the kernel referent but author different epsilon, beneficiary, and victim structures from their own seats.',
    'Under the naturalist sibling the payer polarity reverses: excluded communities and subordinated testimony-holders become the targets and credentialed method becomes the subsidized standard. Cross-reading divergence is therefore expected signal, not error; only convergence across all three readings would license treating the result as kernel-level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_status, conceptual, 'Committer-frame indexicality: classification is authored per reading of a contested kernel.').

omega_variable(
    sufficiency_premise_disagreement_site,
    'Where in the structure does the inter-reading disagreement actually sit, and what would resolving it do to this constraint?',
    'Adjudicate the sufficiency premise through documented cases where material-evidence-only inquiry either validated or violated relational knowledge: Kennewick-Man-class disputes, destructive-sampling controversies, and cases where oral testimony located remains or meanings that instruments missed.',
    'If material evidence alone is judged sufficient, the arrangement reduces to ordinary heritage access regulation, epsilon falls toward coordination-cost territory, and the subordination of credentialed frameworks loses its constitutive warrant. If sufficiency is denied, the subordination is definitional to the reading and the current hybrid structure stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_premise_disagreement_site, conceptual, 'The load-bearing disagreement between readings is the sufficiency of material evidence without transmitted testimony.').

omega_variable(
    oral_tradition_deep_time_fidelity,
    'How reliably does sustained oral tradition transmit verifiable deep-time fact across centuries and millennia?',
    'Geological corroboration studies of dated oral accounts: eruption recollections matched to tephrachronology, descriptions of drowned coastlines matched to sea-level curves, land-bridge accounts matched to bathymetry.',
    'Strong cumulative corroboration raises the credibility of the coordination function and lowers measured extraction, since the testimony channel demonstrably delivers what instruments cannot. Systematic failures in corroborated domains would push the foundational insufficiency axiom toward axiom_overriding drift and destabilize this reading''s authority chain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_tradition_deep_time_fidelity, empirical, 'Empirical basis of the insufficiency axiom: transmission fidelity of oral tradition.').

omega_variable(
    enforcement_hardening_endpoint,
    'Does the enforcement ratchet plateau at administrative compliance (deadlines, inventories, consultation) or continue into prohibition of destructive analysis outright?',
    'Track regulatory revision cycles, disposition-completion statistics, and whether sampling bans migrate from case-by-case assent into blanket category prohibitions.',
    'Continued hardening drives payer-seat extraction upward and dates any transition of this reading toward a snare-shaped endpoint; a plateau at administrative compliance supports the stable hybrid classification authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_hardening_endpoint, empirical, 'Trajectory of enforcement capacity: where the suppression requirement series terminates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 1969, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1969, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1969, 0.15).
narrative_ontology:measurement(anth_tr_t1979, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1979, 0.14).
narrative_ontology:measurement(anth_tr_t1990, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(anth_tr_t1999, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1999, 0.22).
narrative_ontology:measurement(anth_tr_t2008, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(anth_tr_t2017, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2017, 0.21).
narrative_ontology:measurement(anth_tr_t2025, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(anth_be_t1969, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1969, 0.3).
narrative_ontology:measurement(anth_be_t1979, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1979, 0.33).
narrative_ontology:measurement(anth_be_t1990, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(anth_be_t1999, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1999, 0.44).
narrative_ontology:measurement(anth_be_t2008, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2008, 0.46).
narrative_ontology:measurement(anth_be_t2017, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2017, 0.47).
narrative_ontology:measurement(anth_be_t2025, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1969, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1969, 0.22).
narrative_ontology:measurement(anth_su_t1979, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1979, 0.26).
narrative_ontology:measurement(anth_su_t1990, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1990, 0.42).
narrative_ontology:measurement(anth_su_t1999, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1999, 0.55).
narrative_ontology:measurement(anth_su_t2008, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(anth_su_t2017, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2017, 0.61).
narrative_ontology:measurement(anth_su_t2025, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel 'the anthropological record': the colloquial label covers three structurally distinct claims with materially different epsilon values. This reading's referent (oral-tradition-mediated access, community custody) extracts chiefly from credentialed access-holders and holding institutions; the naturalist sibling's referent (open method-gated access) extracts from excluded communities and subordinated testimony traditions; the creationist sibling's referent (scriptural-timeline authority over origins) extracts from method-committed researchers. The naturalist claim is the most empirically established member and is frequently cited as evidence AGAINST this reading's insufficiency axiom, so edges run from this reading into the naturalist sibling as institutional pressure. All three files link mutually through network.affects_constraints; none is orphaned.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
