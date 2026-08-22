% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender Self-Identification Criterion for the Category 'Woman'
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'woman': the
 *   gender-identity reading, under which category membership is determined by
 *   internal identity and 'woman' includes anyone who identifies as a woman
 *   regardless of assigned sex at birth. The sibling readings (sex-biology,
 *   intersex-accommodation) are separate constraints in separate files; per
 *   Rule 1 this story describes only this reading, cleanly and with a single
 *   stable epsilon. The epsilon referent is the standing arrangement under
 *   contest: the self-identification regime as actually instantiated in
 *   documents, services, and eligibility rules, assessed by this reading's
 *   own lights. By those lights the arrangement remedies a real prior
 *   extraction (medical gatekeeping), but justification does not erase
 *   extraction: the reading still counts the costs the arrangement shifts
 *   onto objector seats, and the residual exclusion-extraction in domains
 *   where carve-backs persist, as real. The claim/metric independence rule is
 *   honored: claimed_type is tangled_rope because the structure genuinely
 *   coordinates (uniform dignified recognition) while asymmetrically
 *   extracting (identifiable payers, active enforcement); the metrics are
 *   authored from the arrangement's observable operation, not tuned to any
 *   predicted engine output. Extraction is moderate in document policy and
 *   high in sports and sex-segregated spaces; the blended 0.58 weights toward
 *   the domains where enforcement actually bites, and the dispersion itself
 *   is carried as an omega rather than hidden.
 *
 * KEY AGENTS:
 *   - - transgender_women: Primary beneficiary (organized/identity_locked) — receive recognition and access; cannot exit the category that defines them
 *   - - trans_men_nonbinary_people: Secondary beneficiary (moderate/identity_locked) — ride the same self-declaration architecture
 *   - - female_elite_athletes: Primary payer in competition domains (moderate/constrained) — absorb displaced placements and altered records
 *   - - women_in_single_sex_services: Primary payer in closed settings (powerless/trapped) — bear admission-criteria shifts with no exit
 *   - - gender_critical_campaign_groups: Organized opposition-payer (organized/mobile) — bear speech and employment sanctions, fight via litigation and legislation
 *   - - compelled_speech_objectors: Enforcement-facing payers (moderate/constrained) — comply or object at career cost
 *   - - service_providers_and_employers: Administrator-payer (institutional/constrained) — run the criterion daily, collect nothing
 *   - - equality_lawmakers_and_courts: Agenda-setter (institutional/arbitrage) — extend or carve back the boundary at political cost
 *   - - bioethics_policy_analysts: Analytical observer (analytical/analytical) — sees the full structure, decides nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.58).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.58).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender Self-Identification Criterion for the Category 'Woman'").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '3a520ab9-762d-4941-ab20-d048baadd5d7').
narrative_ontology:cs_kernel_codification('3a520ab9-762d-4941-ab20-d048baadd5d7', distributed).
narrative_ontology:cs_authority_grounding('3a520ab9-762d-4941-ab20-d048baadd5d7', distributed).
narrative_ontology:cs_reading_relation('3a520ab9-762d-4941-ab20-d048baadd5d7', woman_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a520ab9-762d-4941-ab20-d048baadd5d7', woman_category__intersex_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('3a520ab9-762d-4941-ab20-d048baadd5d7', foundational, internal_identity_constitutes_category_membership).
narrative_ontology:cs_axiom_status(internal_identity_constitutes_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('3a520ab9-762d-4941-ab20-d048baadd5d7', internal_identity_constitutes_category_membership, deontological).
narrative_ontology:cs_axiom('3a520ab9-762d-4941-ab20-d048baadd5d7', secondary, biological_gatekeeping_for_recognition_impermissible).
narrative_ontology:cs_axiom_status(biological_gatekeeping_for_recognition_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('3a520ab9-762d-4941-ab20-d048baadd5d7', biological_gatekeeping_for_recognition_impermissible, deontological).
narrative_ontology:cs_reference_frame('3a520ab9-762d-4941-ab20-d048baadd5d7', identity_primacy_recognition).
narrative_ontology:cs_drift_state('3a520ab9-762d-4941-ab20-d048baadd5d7', contemporary_carveback_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3a520ab9-762d-4941-ab20-d048baadd5d7', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, trans_men_nonbinary_people).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_elite_athletes).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, women_in_single_sex_services).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, compelled_speech_objectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, gender_critical_campaign_groups).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, service_providers_and_employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obtain legal documents, service access, and competition entry consistent with their identity under self-declaration, without medical gatekeeping. They also bear the costs of the surrounding contest: heightened scrutiny of every individual placement decision, safety concerns in contested settings, and reputational exposure when high-profile cases drive policy reversals. Leaving the category is not available to them; the category is their identity.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, transgender_women, payer).

% Rely on the same self-declaration architecture for recognition of documents and names. They benefit from the precedent the criterion sets even where their own category placement differs, and have a smaller direct stake in the contests over women's spaces and women's sport.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, trans_men_nonbinary_people, beneficiary,
    moderate, biographical, identity_locked, global).

% Compete for finite podium places, rankings, scholarships, and prize money inside the women's category. Where eligibility criteria admit athletes who have gone through male puberty, they absorb displaced placements, altered record books, and injury-risk exposure. Exiting means retiring from the sport, not relief from the cost.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_elite_athletes, payer,
    moderate, biographical, constrained, global).

% Depend on refuges, prisons, hospital wards, and changing facilities provisioned as single-sex, often at moments of crisis or custody. When admission criteria shift to self-declared identity, they experience the space as no longer exclusively female and bear the resulting discomfort, objection costs, or relocation with almost no individual leverage. Prisoners cannot vote with their feet at all.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, women_in_single_sex_services, payer,
    powerless, biographical, trapped, local).

% Campaign for sex-based legal definitions and robust single-sex exemptions. They bear employment consequences, platform enforcement, and reputational sanction where speech and conduct codes bite, and respond with litigation, legislative lobbying, and media campaigns across jurisdictions. Their leverage is external pressure rather than administration of the criterion.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, gender_critical_campaign_groups, payer,
    organized, generational, mobile, national).

% Employees, clinicians, teachers, and public servants subject to pronoun, record-keeping, and language policies backed by disciplinary process. Their options are compliance, objection at measurable career cost, or finding an employer outside the policy's reach, which for credentialed professions is rarely practical.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, compelled_speech_objectors, payer,
    moderate, biographical, constrained, national).

% Draft admission policies, maintain records, train staff, and defend tribunal claims under equality-law duties that follow them wherever they operate. They administer the criterion daily and absorb compliance and litigation costs without collecting the access gains the criterion distributes.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, service_providers_and_employers, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, service_providers_and_employers, payer).

% Legislate self-identification statutes, issue statutory guidance, and hear challenges. Successive rulings and statutes have both extended the criterion and carved it back in specific domains, making this seat the swing administrator of the boundary, able to redefine it at the cost of political and diplomatic friction rather than personal exposure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, equality_lawmakers_and_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Map the trade-offs across medicine, law, and sport, and publish analyses that both camps cite. They hold no administrative power over the boundary and bear none of its costs; their contribution is the clearest available view of the full structure.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, bioethics_policy_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, self-verifying criterion for assigning the legal and social category 'woman' across documents, services, and institutions: no medical diagnosis, no biological testing, no invasive verification, and no gatekeeper discretion. It solves the problem of recognizing trans people's identities at administrative scale and low cost.
% TRANSFER_FUNCTION: Moves access rights (documents, single-sex spaces, competition categories, record status) to anyone claiming the identity, and moves the corresponding accommodation, compliance, and enforcement costs onto objectors, service providers, and competitors in the affected settings.
% ABSENT_VOICES: Women in closed settings (prisoners, shelter users) had effectively no seat in the policy-drafting rooms where self-ID guidance was written; female athletes were consulted late or narrowly in several federations' initial eligibility decisions; gender-critical academics report employment fear chilling their participation in institutional consultations. These voices enter mainly through litigation and press coverage after policies are set, not before.
% DISAPPEARANCE_RATIONALE: If the self-identification criterion vanished overnight, document registries, prison placement rules, shelter admission policies, and sports eligibility frameworks would all revert to whatever alternative criterion each jurisdiction last used, triggering mass re-registration, renewed medical-gatekeeping regimes, and immediate relitigation of every placement decided under self-ID. The arrangement's disappearance reorganizes a large slice of administrative law and institutional practice.
% FOUNDING_PROBLEM: Legal systems needed a workable, dignified criterion for who counts as a woman that did not require psychiatric diagnosis, hormonal treatment, or sterilization as conditions of recognition, and that allowed trans people to live in documents and daily life consistent with their identity. Pre-self-ID gatekeeping regimes made recognition medically contingent, slow, and humiliating.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the historical record of medical-gatekeeping statutes (sterilization and diagnosis requirements) is documented by legal historians and in human-rights jurisprudence such as Goodwin v United Kingdom and Council of Europe resolutions, none of which originate from trans advocacy organizations. That the problem remains live is further attested by the continued operation of gatekeeping requirements in jurisdictions that never adopted self-ID, and by sports federations' ongoing eligibility reviews, which show the underlying recognition question unresolved even where the criterion is contested.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-high because the arrangement's costs are concentrated rather than diffuse: near-zero friction in document policy, severe in zero-sum competition and closed settings, and the blend weights toward where enforcement operates. Suppression (0.58) is a raw structural property, unscaled by power or scope: it reflects disciplinary processes, employment consequences, and platform enforcement that make dissent costly, and the measurement series deliberately tracks the build-out of that enforcement machinery over the interval, which is why suppression_requirement is authored on the shared grid rather than left static. Theater ratio (0.22) is low: the mechanism functions; the performative share is mostly compliance signaling. Accessibility collapse (0.38) is low-moderate because alternatives demonstrably persist: jurisdictions diverge, courts carve back, federations set domain-specific rules. Resistance (0.72) is among the highest of any construct in the corpus's home domains: the arrangement faces continuous, organized, well-funded contestation from multiple directions simultaneously. All three tracked metrics run on one shared seven-point grid (t=0..24, step 4) so no metric row borrows another's endpoints; the trajectories are monotonic with step-changes at legislative and judicial events rather than cyclical, so no cycle-lengthening was applied.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute very different types from identical metrics. From transgender_women's position the arrangement is recognition itself, and its enforcement is protection; from female_elite_athletes and women_in_single_sex_services the same structure operates as uncompensated dispossession of category and space. Between the two institutional agenda-setter seats the divergence is subtler: equality_lawmakers_and_courts hold arbitrage-grade exit (they can reinterpret or reverse at political cost) while service_providers_and_employers are constrained administrators who bear compliance costs without collecting gains — same power atom, opposite structural relationships, which is why the secondary_role payer declaration matters for the provider seat. Coalition dynamics are live for the powerless seat: women_in_single_sex_services lack individual leverage but supply the constituency that gender_critical_campaign_groups organize and litigate for, which is how a trapped class acquires partial voice.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive transgender_women and trans_men_nonbinary_people toward the beneficiary end of d; identity_locked exit keeps them there (they cannot arbitrage away either the benefit or the scrutiny). Victim declarations drive female_elite_athletes, women_in_single_sex_services, and compelled_speech_objectors toward the target end; trapped and constrained exits keep the first two near full-target, while the organized, mobile campaign groups sit slightly off full-target because their exit options damp the trap. Service providers are the derivation's hardest case: agenda-setters who pay, which the secondary_role encodes so the engine sees both positions. Lawmakers and courts derive low d as agenda-setters with arbitrage exit. No directionality_overrides are authored: the beneficiary/victim plus exit-option data already produces the correct relationships, and the guidance reserves overrides for cases the derivation gets wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents symmetric mislabelings. Reading the arrangement as pure rope would erase the identifiable payers — athletes losing placements, prisoners losing single-sex custody, objectors losing speech — and launder extraction as coordination price. Reading it as pure snare would erase the genuine coordination function — uniform, dignified, administrable recognition that replaced sterilization-and-diagnosis gatekeeping — and treat the primary beneficiaries' net gain as cover. The founding problem is live (corroborated outside the benefiting parties), so mandatrophy_resolved is not declared and no zombie flag is warranted: the arrangement persists because its function persists, not because its function has died behind performance. The honest open question is whether the rope-component and snare-component are separable by domain, which omega domain_epsilon_dispersion routes to decomposition rather than pretending one epsilon settles it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_epsilon_dispersion,
    'Does the single blended epsilon mask a family of domain-specific constraints with materially different extraction profiles (identity documents versus sports eligibility versus closed settings such as prisons and shelters)?',
    'Decompose into per-domain constraint stories (document policy, sports eligibility, sex-segregated services), author each with its own epsilon and stakeholder surface, and compare computed classifications across the family.',
    'If decomposed, the document-policy story likely computes near-rope while the sports and closed-settings stories compute strongly extractive; the current blended value risks under-classifying the sharp domains and over-classifying the mild ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_epsilon_dispersion, conceptual, 'Whether one story can honestly carry the arrangement''s domain-varying extraction.').

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of the kernel ''woman_category''; what structurally changes if a sibling reading (sex_biology_reading or intersex_accommodation_reading) is adopted instead?',
    'Compare the compiled sibling stories: the membership criterion is the disagreement locus, and swapping it swaps the entire beneficiary/victim topology rather than adjusting a parameter.',
    'Under the sex-biology reading the protected class becomes female-born women and transgender women fall outside the category entirely; under the intersex-accommodation reading the category expands along biological variation lines. The same institutional rule extracts from opposite seats depending on the reading adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer-frame routing: reading choice determines victim-set polarity.').

omega_variable(
    bad_faith_declaration_rate_closed_settings,
    'In closed settings (prisons, secure hospitals) where self-declaration governs placement, is the rate of insincere or opportunistic identity declaration material enough to change the extraction profile?',
    'Incident audits and placement-review data from jurisdictions operating self-ID incarceration policies, compared against matched jurisdictions retaining biological placement criteria.',
    'A material bad-faith rate raises effective extraction in closed settings specifically and pushes that domain''s classification toward snare; a negligible rate supports the coordination framing and keeps the blended tangled_rope verdict stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_declaration_rate_closed_settings, empirical, 'Whether self-verification tolerates exploitable entry in coercive settings.').

omega_variable(
    fairness_cost_moral_status,
    'Are the competitive-fairness and privacy costs borne by female athletes and service users extraction (an unjustified imposition) or the unavoidable residue of a tragic trade-off between two legitimate access claims?',
    'Not resolvable by data alone: depends on whether the resolver weights access rights or exclusion rights as lexically prior in zero-sum settings; empirical input (performance data, safety incidents) constrains but does not settle it.',
    'If the costs are judged extraction, the arrangement''s epsilon rises and the payer seats'' classifications harden; if judged tragic-trade-off residue, the same costs are coordination price and the arrangement moves toward rope in those domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fairness_cost_moral_status, preference, 'Access-right versus exclusion-right weighting in zero-sum domains.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of dissent (roughly 0.58) primarily structural (disciplinary process, employment consequence, platform enforcement) or partly internalized (anticipatory self-censorship that would persist if sanctions were removed)?',
    'Post-repeal trajectory test: track dissent expression in jurisdictions or institutions that formally dropped speech codes; persistence of muted expression after sanction removal indicates internalized carryover.',
    'If substantially internalized, effective suppression exceeds the structural measure and would survive formal liberalization; if structural, removing enforcement machinery releases dissent quickly and the constraint''s stability depends wholly on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized component of dissent suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woma_tr_t4, woman_category__gender_identity_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(woma_tr_t8, woman_category__gender_identity_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(woma_tr_t12, woman_category__gender_identity_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(woma_tr_t16, woman_category__gender_identity_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(woma_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(woma_tr_t24, woman_category__gender_identity_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(woma_be_t4, woman_category__gender_identity_reading, base_extractiveness, 4, 0.39).
narrative_ontology:measurement(woma_be_t8, woman_category__gender_identity_reading, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(woma_be_t12, woman_category__gender_identity_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(woma_be_t16, woman_category__gender_identity_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(woma_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(woma_be_t24, woman_category__gender_identity_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(woma_su_t4, woman_category__gender_identity_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(woma_su_t8, woman_category__gender_identity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(woma_su_t12, woman_category__gender_identity_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(woma_su_t16, woman_category__gender_identity_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(woma_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(woma_su_t24, woman_category__gender_identity_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel ('woman'), three readings, three files. This story instantiates the gender-identity reading; the sex-biology and intersex-accommodation readings instantiate different constraints with different victim sets and different epsilon values over the same institutional terrain. Each member links the others via affects_constraints. A second-order decomposition is flagged in omega domain_epsilon_dispersion: within this reading, document-policy, sports-eligibility, and closed-settings applications may warrant separate stories with their own epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
