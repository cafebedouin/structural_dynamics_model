% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gender Category Membership by Self-Declaration (Identity Reading)
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'what grounds
 *   membership in gendered categories': the identity reading, under which
 *   sincere self-declaration is determinative and no body, diagnosis, or
 *   third-party confirmation is required. Under this reading the standing
 *   arrangement under contest is the self-declaration regime as it operates
 *   across documents, facilities, sport, and custodial settings; ε is
 *   authored for THAT arrangement by this reading's own lights — which
 *   concede real gatekeeping and boundary costs — not for the biological or
 *   role-based alternatives it displaces (those are sibling constraints,
 *   linked via network.affects_constraints). The colloquial label 'what makes
 *   someone a woman or man' decomposes into three structurally distinct
 *   constraints per the ε-invariance principle; this file is one member of
 *   that family. The claim/metric gap is deliberate: the reading CLAIMS
 *   tangled_rope (genuine coordination in dignified, low-friction
 *   recognition, with real but bounded extraction) while the metrics describe
 *   moderately extractive, actively enforced operation with hardening
 *   suppression of dissent — the engine measures the divergence; do not
 *   reconcile them.
 *
 * KEY AGENTS:
 *   - - trans_people_self_identifying: Primary beneficiary (organized/constrained) — receive recognition and access via declaration; cannot reverse course cheaply
 *   - - cis_women_in_affected_spaces: Primary target (moderate/constrained) — bear the boundary shift without individual consent; objection carries reputational price
 *   - - single_sex_service_operators: Local administrator (institutional/constrained) — enforce the rule, absorb bidirectional grievances and legal exposure
 *   - - equality_lawmakers: Agenda setter (institutional/mobile) — codified the rule and could amend it at political cost
 *   - - gender_category_administrators: Secondary beneficiary (institutional/mobile) — shed case-by-case adjudication burden, absorb complaint load
 *   - - gender_critical_dissenters: Sanctioned dissenter (organized/constrained) — bear suppression and report consultative exclusion
 *   - - bioethics_analysts: Analytical observer — sees the full structure including the sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.5).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.62).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership by Self-Declaration (Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '30b358c4-c39f-4a05-8ac3-9f37eaca1956').
narrative_ontology:cs_kernel_codification('30b358c4-c39f-4a05-8ac3-9f37eaca1956', distributed).
narrative_ontology:cs_authority_grounding('30b358c4-c39f-4a05-8ac3-9f37eaca1956', lineage).
narrative_ontology:cs_interpretation_layer_present('30b358c4-c39f-4a05-8ac3-9f37eaca1956').
narrative_ontology:cs_reading_relation('30b358c4-c39f-4a05-8ac3-9f37eaca1956', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('30b358c4-c39f-4a05-8ac3-9f37eaca1956', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('30b358c4-c39f-4a05-8ac3-9f37eaca1956', foundational, self_declared_identity_constitutes_membership).
narrative_ontology:cs_axiom_status(self_declared_identity_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('30b358c4-c39f-4a05-8ac3-9f37eaca1956', self_declared_identity_constitutes_membership, deontological).
narrative_ontology:cs_axiom('30b358c4-c39f-4a05-8ac3-9f37eaca1956', secondary, external_refusal_cannot_override_avowal).
narrative_ontology:cs_axiom_status(external_refusal_cannot_override_avowal, holdable).
narrative_ontology:cs_axiom_grounding('30b358c4-c39f-4a05-8ac3-9f37eaca1956', external_refusal_cannot_override_avowal, deontological).
narrative_ontology:cs_reference_frame('30b358c4-c39f-4a05-8ac3-9f37eaca1956', self_declaration_as_determinative).
narrative_ontology:cs_drift_state('30b358c4-c39f-4a05-8ac3-9f37eaca1956', post_fws_litigation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('30b358c4-c39f-4a05-8ac3-9f37eaca1956', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_people_self_identifying).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_category_administrators).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_in_affected_spaces).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, gender_critical_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, single_sex_service_operators).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, gender_category_administrators).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, self_identification_principle).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, depathologization_doctrine).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, yogyakarta_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain recognition of their identified gender across documents, facilities, teams, and services by declaring it, with no requirement of medical diagnosis, treatment history, or third-party confirmation. What previously ran through clinics and tribunals now runs through a declaration others are obliged to honor. Stepping back from the identification would mean surrendering recognition already secured, so the position is durable but not freely reversible.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_people_self_identifying, beneficiary,
    organized, biographical, constrained, global).

% Use shelters, refuges, prisons, changing rooms, and sporting categories whose admission criteria have shifted from anatomy at birth to declared identity. No individual consented to the shift and no individual can withdraw from needing these spaces; the practical choice is between using them under the new rule and going without. Public objection draws the charge of exclusion, which raises the personal price of voicing the concern.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_in_affected_spaces, payer,
    moderate, biographical, constrained, national).

% Run shelters, prison estates, sports competitions, and schools where the admission boundary sits. They rewrite admission policy, field grievances from residents denied company they expected and from applicants denied entry, and carry legal exposure under discrimination law whichever way a borderline case is decided. Regulators and funders expect compliance; opting out of enforcement is not available to them.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, single_sex_service_operators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, single_sex_service_operators, payer).

% Enact and amend the recognition statutes and equality-law definitions that make declaration decisive, and draw the exception lines that preserve some anatomy-based admissions in prisons, sport, and intimate services. Courts, other legislatures, and mobilized campaigns on both sides press on those lines; revisiting the statute is procedurally open but politically expensive.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, equality_lawmakers, agenda_setter,
    institutional, generational, mobile, national).

% Operate the paperwork: registers, HR systems, team sheets, ward allocations. Declaration removes the older burden of collecting diagnoses or judging sincerity case by case, which saves adjudication work, and substitutes complaint intake, guidance updates, and staff training when decisions are contested. Their horizon is the budget and audit cycle.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_category_administrators, beneficiary,
    institutional, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_category_administrators, payer).

% Argue for anatomy-based or role-based criteria and for keeping some spaces admission-controlled. They absorb professional complaints, platform demotion, lost commissions, and social ostracism for saying so publicly, and report that consultation processes gave their submissions little weight. Standing down would mean abandoning a position they treat as defending women's provisions, so they continue despite accumulating costs.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_critical_dissenters, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, gender_critical_dissenters, excluded).

% Map which jurisdictions adopted which criterion, track litigation outcomes, and weigh dignity, privacy, fairness, and safety claims against each other across the rival criteria. They publish, testify, and advise; they neither administer the rule nor live under its boundary decisions.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, bioethics_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, trans_people_self_identifying).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every institution that sorts people by gender a single low-friction membership rule — declaration — replacing case-by-case adjudication of bodies, diagnoses, or performances, so records, documents, and facility rosters stay consistent without tribunals.
% TRANSFER_FUNCTION: Moves decision authority over the category boundary from co-members and service operators to each declarant; moves dispute-resolution work, legal exposure, and reputational risk onto operators and public dissenters; moves recognition and access to those who declare.
% ABSENT_VOICES: Detained women and shelter residents have no independent lobbying voice and were represented only indirectly by service providers. Gender-critical organisations report that consultation submissions received minimal acknowledgment before rules were finalized. Children subject to school-level policies had no formal voice. Their absence makes the consensus behind the rule thinner than published guidance implies.
% DISAPPEARANCE_RATIONALE: If the declaration rule vanished overnight, registry entries, facility admission policies, sporting categories, and thousands of administrative determinations would revert to whatever criterion each operator last used; the recognition status of people who transitioned under the rule becomes indeterminate; the sanction machinery disciplining dissent dissolves. Arrangements across law, sport, health, and incarceration demonstrably depend on it.
% FOUNDING_PROBLEM: Recognition of a trans person's gender previously required medicalized gatekeeping — psychiatric diagnosis, mandatory treatment or sterilization in some jurisdictions, years-long tribunals — which made recognition humiliating, slow, and inaccessible to those who could not or would not undergo it.
% FOUNDING_PROBLEM_CORROBORATION: Attested outside the beneficiary set: the European Court of Human Rights (A.P., Garçon and Nicot v. France, 2017) held sterilization requirements for recognition violated Article 8; WHO's ICD-11 (2019) moved gender incongruence out of mental-disorder classifications; Council of Europe human rights bodies documented abusive gatekeeping practices. Against this, gender-critical legal scholarship and the UK Supreme Court's 2025 judgment in For Women Scotland argue the currently disputed questions concern space allocation rather than the founding problem — hence contested, not live or dead.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.50): the rule transfers boundary authority and imposes uncompensated adjustment costs on occupants of affected spaces, but it simultaneously removes genuinely burdensome medicalized gatekeeping, so extraction is real yet bounded. Suppression is higher than extraction (0.62, raw structural property, unscaled by power or scope — only extraction is scaled in the engine's computation): persistence depends on discrimination-law exposure, professional discipline, platform moderation, and the social positioning of resisters as exclusionaries, not on voluntary uptake. Theater ratio is modest (0.28): the recognition function is real, but a growing share of institutional activity — training modules, pledge displays, compliance signaling — performs adherence rather than producing it. Accessibility collapse is low-moderate (0.35): the sibling readings remain operative in many jurisdictions and courts, so alternatives have not closed. Resistance is high (0.65): litigation, rival legislation, and organized campaigning meet the rule continuously. All three tracked series run on one shared grid (points 0,2,4,6,8,10,12,14); the rising suppression series traces enforcement machinery maturing over the interval, which is why suppression_requirement is tracked rather than left static.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the lawmaker and administrator positions the arrangement is a coordination improvement they built and operate: adjudication burden fell, records stayed consistent. From the cis-women and dissenter positions the same structure operates as an imposed transfer of boundary authority plus a sanction regime attached to objection. From the trans-person position it is a benefit received without administering anything. The engine derives these divergent per-seat classifications from the structural data (power, exit, directionality); the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for trans_people_self_identifying (full subsidy end: recognition and access flow in, costs are diffuse and borne elsewhere) and for gender_category_administrators (adjudication savings, mobile exit, though complaint load pulls them slightly off the floor — captured via their dual beneficiary/payer role rather than an override). Victims drive high directionality for cis_women_in_affected_spaces and gender_critical_dissenters: constrained exit amplifies both toward the full-target end, since neither can leave the spaces or the discourse the rule governs. single_sex_service_operators sit near symmetric — they enforce and bear costs without collecting the gains — expressed through the agenda_setter-plus-payer dual role. equality_lawmakers derive a middling d: they set the rule and could revise it, collecting political rather than material rents. No directionality_overrides are needed: the derivation chain produces these values from the declared beneficiary/victim structure and exit options, and the override surface is keyed by power atom, which would misapply a correction across unrelated institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — humiliating, inaccessible medicalized gatekeeping — is substantially solved where the rule applies, but the arrangement has extended past that mandate into space allocation, sport, and custody, where the founding problem never reached. Hence founding_problem_status is contested, not dead: the problem is gone for documents, live-again for spaces, and the parties dispute which frame governs. The contested-status x world_rearranges combination avoids the dead-mandate zombie flag while still recording that the arrangement now does work its genealogy does not cover. Classification-wise, the analysis blocks both mislabels: calling this a pure snare erases the real coordination achievement (depathologized recognition that courts and medical bodies corroborated as solving a genuine harm), and calling it a pure rope erases the asymmetric transfer and the sanction regime bearing down on dissenters. Tangled rope preserves both halves; the moderate ε, elevated suppression, and persistent resistance are the signature of that hybrid, not noise to be tuned away.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the gender_identity_reading of the kernel gendered_category_membership (''what grounds membership in gendered categories?''). Sibling readings — biological_sex_reading (immutable birth markers) and social_role_reading (sustained performance and recognition by others) — instantiate different constraints with different victim sets and different ε. Which reading a jurisdiction adopts determines who bears exclusion costs and who counts as the perpetrator of exclusion; the disagreement is located in the criterion-of-membership premise itself, not in peripheral applications.',
    'Comparative jurisdictional analysis plus appellate jurisprudence tracing which criterion each legal order treats as determinative, and whether any single framework can hold two criteria without splitting into separate constraints.',
    'Classification of space-allocation and eligibility arrangements flips with the adopted reading: under this reading, resisting cis women occupy the exclusion-perpetrator position; under biological_sex_reading the victim set inverts. Per-seat classifications computed from this file are valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    hybrid_framework_stability,
    'Does self-declaration-as-determinative logically foreclose the biological-marker criterion in every framework, or can stable two-tier frameworks (declaration decisive for documents and ordinary services, anatomy decisive for prisons and elite sport) hold both without contradiction?',
    'Test whether any enduring legal order assigns both criteria to the same category without instability; observe whether exception clauses creep or stabilize.',
    'If hybrid frameworks prove stable, the forecloses edge toward biological_sex_reading weakens toward influence; if they collapse back toward one criterion, the foreclosure holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_framework_stability, conceptual, 'Whether the foreclosure relation between identity and biology criteria is absolute or framework-relative.').

omega_variable(
    harm_incidence_empirics,
    'What are the measured incidence rates of the harms asserted on each side — safety and privacy incidents in facilities opened by declaration versus wellbeing harms from denial of recognition under gatekeeping?',
    'Longitudinal incident studies in jurisdictions that changed admission rules, matched against wellbeing and suicide-risk outcome studies in gatekept versus self-declaration regimes.',
    'Negligible facility harms would drive effective extraction down toward a pure coordination reading; substantial verified harms would drive it up toward a capture-dominated reading. Either resolution moves ε materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_incidence_empirics, empirical, 'Empirical distribution of harms underlying both advocacy narratives.').

omega_variable(
    dissent_suppression_mechanism,
    'Is the suppression of dissent structural (employment law, professional regulation, platform rules, consultation design) or internalized (anticipatory fear of stigma that persists where formal rules permit dissent)?',
    'Post-reversal trajectory: where legal risk was removed by court ruling or statute change, observe whether gender-critical publication and employment rebound; persistence of self-censorship after barrier removal indicates internalization.',
    'If largely structural, removing the enforcement machinery restores dissent quickly and the suppression measure tracks the rule''s active force; if internalized, effective suppression exceeds the structural measure and outlives the rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissent_suppression_mechanism, empirical, 'Structural versus internalized composition of measured dissent suppression.').

omega_variable(
    relocated_gatekeeping_costs,
    'Does self-declaration eliminate gatekeeping or relocate it onto narrower domains (sporting eligibility panels, prison categorization reviews) where verification re-enters?',
    'Audit where evidence demands persist after adoption: compare pre/post administrative burden in documents and general services against eligibility panels in sport and custodial settings.',
    'Full relocation means the coordination saving is smaller than claimed and residual gatekeeping concentrates on the least powerful applicants; genuine elimination supports the coordination-function reading and lowers theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relocated_gatekeeping_costs, empirical, 'Whether gatekeeping was abolished or displaced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gend_tr_t0, observed).
narrative_ontology:measurement(gend_tr_t2, gendered_category_membership__gender_identity_reading, theater_ratio, 2, 0.13).
narrative_ontology:measurement_basis(gend_tr_t2, observed).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__gender_identity_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement_basis(gend_tr_t4, observed).
narrative_ontology:measurement(gend_tr_t6, gendered_category_membership__gender_identity_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(gend_tr_t6, observed).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__gender_identity_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(gend_tr_t8, observed).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__gender_identity_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(gend_tr_t10, observed).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__gender_identity_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement_basis(gend_tr_t12, observed).
narrative_ontology:measurement(gend_tr_t14, gendered_category_membership__gender_identity_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement_basis(gend_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(gend_be_t0, observed).
narrative_ontology:measurement(gend_be_t2, gendered_category_membership__gender_identity_reading, base_extractiveness, 2, 0.33).
narrative_ontology:measurement_basis(gend_be_t2, observed).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__gender_identity_reading, base_extractiveness, 4, 0.36).
narrative_ontology:measurement_basis(gend_be_t4, observed).
narrative_ontology:measurement(gend_be_t6, gendered_category_membership__gender_identity_reading, base_extractiveness, 6, 0.39).
narrative_ontology:measurement_basis(gend_be_t6, observed).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__gender_identity_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(gend_be_t8, observed).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__gender_identity_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(gend_be_t10, observed).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__gender_identity_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement_basis(gend_be_t12, observed).
narrative_ontology:measurement(gend_be_t14, gendered_category_membership__gender_identity_reading, base_extractiveness, 14, 0.5).
narrative_ontology:measurement_basis(gend_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gend_su_t0, observed).
narrative_ontology:measurement(gend_su_t2, gendered_category_membership__gender_identity_reading, suppression_requirement, 2, 0.38).
narrative_ontology:measurement_basis(gend_su_t2, observed).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__gender_identity_reading, suppression_requirement, 4, 0.41).
narrative_ontology:measurement_basis(gend_su_t4, observed).
narrative_ontology:measurement(gend_su_t6, gendered_category_membership__gender_identity_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(gend_su_t6, observed).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__gender_identity_reading, suppression_requirement, 8, 0.49).
narrative_ontology:measurement_basis(gend_su_t8, observed).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__gender_identity_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement_basis(gend_su_t10, observed).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__gender_identity_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(gend_su_t12, observed).
narrative_ontology:measurement(gend_su_t14, gendered_category_membership__gender_identity_reading, suppression_requirement, 14, 0.62).
narrative_ontology:measurement_basis(gend_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% Family decomposition per the ε-invariance principle: the colloquial label 'grounds of gendered category membership' covers three structurally distinct constraints with distinct ε, victim sets, and failure modes. The biological reading is the inherited baseline (upstream, historically dominant); the identity reading creates structural pressure on it (statutory displacement, litigation) and on the social-role reading (whose recognition-by-others criterion this reading partially absorbs as mere evidence while denying it constitutive force). Each member links the others via network.affects_constraints; no member averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
