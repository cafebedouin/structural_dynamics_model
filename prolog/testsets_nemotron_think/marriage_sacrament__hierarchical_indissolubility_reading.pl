% ============================================================================
% CONSTRAINT STORY: marriage_sacrament__hierarchical_indissolubility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_sacrament__hierarchical_indissolubility_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_sacrament__hierarchical_indissolubility_reading
 *   human_readable: Sacramental Marriage Indissolubility with Hierarchical Adjudication
 *   domain: religious/canonical/sociological
 *
 * SUMMARY:
 *   This constraint story instantiates the hierarchical indissolubility
 *   reading of the contested marriage_sacrament kernel. The reading treats
 *   marriage as an ontological reality — a sacrament that creates an
 *   indissoluble bond — whose validity can only be adjudicated by the
 *   ecclesiastical hierarchy through canonical tribunals. The constraint
 *   operates through the 1983 Code of Canon Law (canons 1055-1165): marriage
 *   is presumed valid until proven null; divorced Catholics who civilly
 *   remarry without a declaration of nullity are objectively in a state of
 *   adultery and cannot receive the Eucharist (canon 915); the annulment
 *   process requires petition, evidence, defender of the bond, and appellate
 *   review, imposing years of delay and substantial fees. The coordination
 *   function is genuine: it provides an objective, unified standard for
 *   sacramental marriage across the global Church, protecting the bond from
 *   subjective dissolution. The extraction function is asymmetric:
 *   divorced/remarried Catholics bear the cost of exclusion from the
 *   Eucharist and the burden of proving nullity; tribunal personnel and
 *   canonical lawyers collect fees and professional standing; the hierarchy
 *   retains exclusive adjudicative authority. The civic_pastoral_reading
 *   (sibling) treats indissolubility as an ideal requiring compassionate
 *   discernment in individual cases — a different constraint with different
 *   beneficiary/victim structure.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, 0.82).
domain_priors:suppression_score(marriage_sacrament__hierarchical_indissolubility_reading, 0.78).
domain_priors:theater_ratio(marriage_sacrament__hierarchical_indissolubility_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_sacrament__hierarchical_indissolubility_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_sacrament__hierarchical_indissolubility_reading, tangled_rope).
narrative_ontology:human_readable(marriage_sacrament__hierarchical_indissolubility_reading, "Sacramental Marriage Indissolubility with Hierarchical Adjudication").
narrative_ontology:topic_domain(marriage_sacrament__hierarchical_indissolubility_reading, "religious/canonical/sociological").

domain_priors:requires_active_enforcement(marriage_sacrament__hierarchical_indissolubility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_sacrament__hierarchical_indissolubility_reading, 'c85a0861-c4cd-40d8-9e5e-bc7d639088a8').
narrative_ontology:cs_kernel_codification('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', formalized).
narrative_ontology:cs_authority_grounding('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', lineage).
narrative_ontology:cs_interpretation_layer_present('c85a0861-c4cd-40d8-9e5e-bc7d639088a8').
narrative_ontology:cs_reading_relation('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', marriage_sacrament__civic_pastoral_reading, coexists_with).
narrative_ontology:cs_axiom('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', foundational, sacramental_indissolubility_ontological).
narrative_ontology:cs_axiom_status(sacramental_indissolubility_ontological, holdable).
narrative_ontology:cs_axiom_grounding('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', sacramental_indissolubility_ontological, deontological).
narrative_ontology:cs_axiom('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', foundational, hierarchical_adjudication_exclusive).
narrative_ontology:cs_axiom_status(hierarchical_adjudication_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', hierarchical_adjudication_exclusive, conventional).
narrative_ontology:cs_reference_frame('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', tridentine_canonical_framework).
narrative_ontology:cs_drift_state('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', post_amoris_laetitia_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c85a0861-c4cd-40d8-9e5e-bc7d639088a8', '').
narrative_ontology:cs_kernel_id(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, institutional_church_hierarchy).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunals).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_theology_establishment).
narrative_ontology:constraint_beneficiary(marriage_sacrament__hierarchical_indissolubility_reading, canonical_lawyers).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics).
narrative_ontology:constraint_victim(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_indissolubility).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, hierarchical_adjudication_authority).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, ontological_marriage_reality).
narrative_ontology:constraint_vindicates(marriage_sacrament__hierarchical_indissolubility_reading, eucharistic_coherence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Roman Pontiff and the College of Bishops in communion with him. They promulgate and interpret the Code of Canon Law, appoint tribunal judges, and reserve the authority to grant dispensations or modify norms. They collect the authority to define sacramental marriage for 1.3 billion Catholics. Exit is arbitrage: they could change the law (as Pope Francis did with Mitis Iudex Dominus Iesus, 2015) but face schism risk and doctrinal precedent constraints.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, institutional_church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, universal).

% Diocesan and interdiocesan tribunals, the Roman Rota, the Apostolic Signatura. They process annulment petitions (40,000+ annually in US at peak), collect fees ($500-$2000 per case, often waived for indigence), and exercise delegated judicial authority. Their professional existence depends on the annulment system. Exit is constrained: canon lawyers can move to civil law but lose ecclesiastical office; tribunal structures are embedded in diocesan governance.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunals, beneficiary,
    organized, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, canonical_tribunals, agenda_setter).

% Pontifical universities, theological faculties, the Congregation for the Doctrine of the Faith (now Dicastery for the Doctrine of the Faith), canonical institutes. Their intellectual capital — publications, curricula, magisterial advisory roles — is built on the indissolubility ontology. Exit is identity-locked: their professional self-concept is constituted by defending this reading; dissent risks loss of canonical mission (missio canonica) and ecclesial standing.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, sacramental_theology_establishment, beneficiary,
    institutional, generational, identity_locked, universal).

% Advocates, procurators, and canon law professors who represent petitioners in annulment cases. They charge professional fees (often $2,000-$10,000 per case) and hold a monopoly on competent representation before tribunals. Exit is mobile: they can practice civil family law, but the canonical niche provides steady demand and professional prestige within the Church.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, canonical_lawyers, beneficiary,
    moderate, biographical, mobile, universal).

% Catholics who have divorced, civilly remarried without a declaration of nullity, and seek full sacramental participation. They are objectively excluded from the Eucharist (canon 915) and often from liturgical ministries. Exit is identity-locked: leaving the Catholic Church means abandoning the sacramental worldview that constitutes their spiritual identity; staying means accepting second-class communion. Many disengage entirely (contributing to the 'nones' demographic); some pursue annulments despite low hope of success; a few receive Communion contrary to canon, risking scandal.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, divorced_remarried_catholics, payer,
    powerless, biographical, identity_locked, universal).

% Catholics petitioning for a declaration of nullity for a prior marriage. They bear the burden of proof, tribunal fees (even if subsidized), and 12-24 month delays (longer in complex cases). The defender of the bond argues for the marriage's validity. Exit is constrained: they can abandon the petition (losing fees and time) or appeal an adverse decision (more time, cost); they cannot unilaterally dissolve the bond or receive Communion while remarried.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, annulment_petitioners, payer,
    moderate, immediate, constrained, universal).

% Parish priests, deacons, lay ecclesial ministers who accompany divorced/remarried Catholics pastorally but lack authority to change the norm. They witness the human cost of exclusion, administer the internal forum (pastoral discernment) that Amoris Laetitia footnote 352 opened, but cannot grant Eucharistic access. Their exclusion is structural: they would object to the rigidity if empowered, but their role is implementation, not adjudication.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_ministers, excluded,
    moderate, biographical, constrained, universal).
narrative_ontology:stakeholder_secondary_role(marriage_sacrament__hierarchical_indissolubility_reading, pastoral_ministers, observer).

% Moral theologians (e.g., Bernhard Häring, Margaret Farley, James Keenan), canonists (e.g., Ladislas Örsy, John Huels), and episcopal conferences (e.g., German Synodal Way, Belgian bishops) who argue for pastoral discernment over juridical rigor. They are excluded from magisterial authority; their publications face doctrinal review; some have lost canonical missions. Exit is constrained: they remain Catholic theologians contesting from within, not forming alternative magisteria.
narrative_ontology:constraint_stakeholder(marriage_sacrament__hierarchical_indissolubility_reading, theological_dissenters, excluded,
    moderate, biographical, constrained, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, objective, universal framework for sacramental marriage that protects the indissoluble bond from subjective dissolution, ensuring sacramental integrity and ecclesial unity through hierarchical adjudication rather than individual conscience or civil law.
% TRANSFER_FUNCTION: Moves the authority to determine marriage validity from the couple to the ecclesiastical hierarchy; moves sacramental access (Eucharist) from divorced/remarried persons to those with canonically valid marriages; moves tribunal fees, time costs, and evidentiary burdens from the institution to petitioners; moves interpretive authority from local pastors to Roman tribunals and the CDF/DDF.
% ABSENT_VOICES: Divorced and remarried Catholics who seek full communion but are canonically excluded (est. millions globally); laity who experience the annulment process as opaque, costly, and traumatizing; theologians and pastoral ministers advocating for discernment over juridical rigor; Orthodox Christians whose ekonomia permits second marriages — their praxis demonstrates an alternative coordination model but they are not in the Catholic canonical conversation.
% DISAPPEARANCE_RATIONALE: The entire canonical marriage system (canons 1055-1165), the global tribunal network (hundreds of tribunals, thousands of personnel), the Eucharistic admission discipline for the divorced/remarried, and the canonical lawyer profession are built on this constraint. Its removal would reorganize Catholic sacramental practice, ecclesiastical judicial structures, and the theology of marriage — a structural rearrangement comparable to the 1983 Code's promulgation but in reverse.
% FOUNDING_PROBLEM: The need to protect the sacramental character of marriage from cultural divorce practices and subjective dissolution, preserving the indissoluble bond as a sign of Christ's union with the Church (Eph 5:32), and preventing the privatization of marital validity that Protestant reform and civil divorce introduced.
% FOUNDING_PROBLEM_CORROBORATION: The Council of Trent (Session 24, 1563) and the 1917/1983 Codes of Canon Law attest the founding problem from the institutional seat. The 2014-2015 Synods on the Family, the post-synodal exhortation Amoris Laetitia (2016), and the 2023-2024 Synod on Synodality attest the contestation from episcopal and pastoral seats. Sociological data (CARA, Pew, GSS) on Catholic divorce rates matching secular rates, annulment grant rates approaching 90% in the US (1990s-2010s), and declining annulment petitions corroborate the shifted social reality from outside the benefiting parties.
narrative_ontology:disappearance_verdict(marriage_sacrament__hierarchical_indissolubility_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_sacrament__hierarchical_indissolubility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_sacrament__hierarchical_indissolubility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_sacrament__hierarchical_indissolubility_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_sacrament__hierarchical_indissolubility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_sacrament__hierarchical_indissolubility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint moves sacramental access from a large population (divorced/remarried Catholics, est. 4-6 million in US alone) to the institutionally compliant, while tribunal fees and delays extract time and money from petitioners. Suppression (0.78) is high because canonical penalties (exclusion from Eucharist, canon 915) and the monopoly on validity adjudication actively suppress alternatives — civil remarriage, Orthodox ekonomia, Protestant recognition of divorce. Theater ratio (0.38) is moderate: the pastoral accompaniment rhetoric (Amoris Laetitia, footnote 352) creates performative mitigation while the juridical structure remains unchanged. Accessibility collapse (0.72) is high for victims: leaving the Catholic Church for another communion is identity-locked for cradle Catholics; staying means accepting exclusion. Resistance (0.48) is moderate: organized dissent exists (We Are Church, Call to Action, some episcopal conferences) but lacks structural power to change the constraint.
 *
 * PERSPECTIVAL GAP:
 *   The hierarchy (agenda_setter) experiences this as genuine coordination protecting sacramental truth; divorced/remarried Catholics (payers) experience it as enforced exclusion from the source of grace; tribunal officials (beneficiaries) experience it as professional vocation with institutional rewards; pastoral ministers (excluded/observer) experience tension between juridical norm and pastoral reality. The engine computes per-seat classification from these structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional hierarchy (d ≈ 0.15): full beneficiary — sets rules, collects authority, faces no extraction. Canonical tribunals (d ≈ 0.2): beneficiary — collect fees, exercise delegated authority, constrained by higher hierarchy. Sacramental theology establishment (d ≈ 0.1): beneficiary — intellectual capital tied to the reading's dominance. Canonical lawyers (d ≈ 0.25): beneficiary — professional rents from annulment petitions. Divorced/remarried Catholics (d ≈ 0.9): full target — bear exclusion, identity-locked exit. Annulment petitioners (d ≈ 0.8): target — bear costs, delays, uncertainty; constrained exit (can petition but cannot unilaterally dissolve). Pastoral ministers (d ≈ 0.5): symmetric — enforce constraint they may pastorally contest. Theological dissenters (d ≈ 0.7): target — marginalized, constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting sacramental marriage from cultural divorce) remains live per hierarchy but is contested per sociological reality: Catholic divorce rates mirror secular rates; annulment grants approach 90% in US, suggesting the constraint's coordination function (preventing subjective dissolution) has atrophied while extraction (tribunal fees, Eucharistic exclusion) persists. This is a Tangled Rope, not a Snare, because the coordination function (unified sacramental standard) is genuinely believed by beneficiaries and structurally real — but the extraction layered atop it is substantial and asymmetric. The mandatrophy is unresolved: the arrangement persists because the hierarchy could change it (cheap fixing_cost for them) but the cost to fix (doctrinal rupture, schism risk) is prohibitive relative to their benefit of maintaining adjudicative monopoly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the hierarchical indissolubility reading a genuine coordination mechanism for sacramental integrity, or an extractive structure that instrumentalizes ontology to maintain hierarchical authority?',
    'Compare sacramental outcomes and lay adherence in jurisdictions where pastoral discernment (civic_pastoral_reading) has been authorized versus those maintaining strict juridical adjudication; track whether Eucharistic coherence correlates with adjudicative rigidity.',
    'If coordination is genuine, relaxation of adjudication should correlate with sacramental incoherence; if extractive, relaxation should not degrade sacramental life but would reduce institutional control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the reading''s ontological claim functions as coordination or extraction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the exclusion of divorced/remarried Catholics from the Eucharist experienced as structural coercion (canonical penalty) or internalized suppression (identity fusion with ecclesial norm)?',
    'Longitudinal study of divorced/remarried Catholics who leave the Church versus those who remain but disengage from Eucharist; measure suppression persistence after formal exit.',
    'If internalized, effective suppression exceeds canonical text; the constraint operates through identity-lock beyond institutional reach.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression for the primary victim group.').

omega_variable(
    annulment_process_extraction_boundary,
    'Do tribunal fees and procedural delays reflect genuine adjudicative cost or rent extraction from a captive petitioner population?',
    'Cost accounting of tribunal operations versus fee revenue; comparison with secular family court processing times and costs for similar evidentiary complexity.',
    'If fees exceed cost and delays exceed complexity justification, the annulment process is an extraction mechanism layered onto the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(annulment_process_extraction_boundary, empirical, 'Whether annulment tribunals extract rents beyond coordination costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_sacrament__hierarchical_indissolubility_reading, 1983, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mshird_tr_t1983, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1983, 0.25).
narrative_ontology:measurement(mshird_tr_t1995, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(mshird_tr_t2005, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement(mshird_tr_t2015, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(mshird_tr_t2024, marriage_sacrament__hierarchical_indissolubility_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(mshird_be_t1983, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1983, 0.65).
narrative_ontology:measurement(mshird_be_t1995, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(mshird_be_t2005, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2005, 0.75).
narrative_ontology:measurement(mshird_be_t2015, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2015, 0.8).
narrative_ontology:measurement(mshird_be_t2024, marriage_sacrament__hierarchical_indissolubility_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(mshird_su_t1983, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1983, 0.65).
narrative_ontology:measurement(mshird_su_t1995, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(mshird_su_t2005, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(mshird_su_t2015, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(mshird_su_t2024, marriage_sacrament__hierarchical_indissolubility_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_sacrament__hierarchical_indissolubility_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_sacrament__hierarchical_indissolubility_reading, 0.08).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, catholic_sacramental_discipline).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, canonical_annulment_process).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, eucharistic_admission_norm).
narrative_ontology:affects_constraint(marriage_sacrament__hierarchical_indissolubility_reading, marriage_sacrament__civic_pastoral_reading).

% DUAL FORMULATION NOTE:
% This constraint and its sibling civic_pastoral_reading form the marriage_sacrament constraint family. They share the kernel (the Church's claim to regulate sacramental marriage) but instantiate different constraints with different ε: this reading ε≈0.82 (high extraction via exclusion and tribunal costs); the pastoral reading would have lower ε (reduced exclusion, streamlined discernment). The upstream reading (this one) influences the downstream: the hierarchy's adjudicative monopoly is cited as the reason pastoral discernment cannot be universally authorized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, moderate, 0.5).
constraint_indexing:directionality_override(marriage_sacrament__hierarchical_indissolubility_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
