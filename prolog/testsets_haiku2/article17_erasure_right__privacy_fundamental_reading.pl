% ============================================================================
% CONSTRAINT STORY: article17_erasure_right__privacy_fundamental_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article17_erasure_right__privacy_fundamental_reading, []).

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
 *   constraint_id: article17_erasure_right__privacy_fundamental_reading
 *   human_readable: GDPR Article 17 Right to Erasure (Privacy Fundamental Reading)
 *   domain: technology/legal/individual_rights
 *
 * SUMMARY:
 *   Article 17 of the GDPR (the Right to be Forgotten / Right to Erasure)
 *   grants individuals the legal right to request deletion of their personal
 *   data from platforms and services under specified conditions: when the
 *   data is no longer necessary for its original purpose, when the individual
 *   withdraws consent, or when processing is illegal. This is ONE READING of
 *   a contested kernel. The privacy_fundamental_reading frames Article 17 as
 *   instantiating individual data sovereignty as a fundamental human right
 *   that rebalances power between data subjects and institutional collectors.
 *   From this reading's perspective, individuals recover agency over their
 *   digital footprint; extraction is constrained because the individual
 *   retains deletion leverage; compliance cost is borne by platforms as a
 *   correction for historical imbalance. The sibling readings
 *   (competitive_moat_reading, censorship_mechanism_reading) contest this
 *   framing: they assert that Article 17 functions as incumbent protection
 *   via compliance cost asymmetry, or as a weapon for strategic content
 *   suppression via erasure requests. The claim/metric gap is intentional:
 *   this reading CLAIMS the constraint is a genuine coordination rope
 *   (solving individual powerlessness) while the authored metrics show
 *   moderate extraction (0.31) and modest suppression (0.18) — the extraction
 *   exists because compliance is costly and asymmetric, and suppression
 *   emerges because platforms resist deletion requests. The engine will
 *   compute divergent per-seat classifications: from the data subject seat,
 *   the constraint appears as genuine coordination with low extraction; from
 *   the platform seat, it appears as enforced asymmetric compliance cost.
 *   This divergence is the measurement.
 *
 * KEY AGENTS:
 *   - data_subjects: powerless individuals seeking deletion leverage
 *   - online_platforms: institutional collectors bearing compliance cost
 *   - data_protection_authorities: institutional arbiters of erasure scope
 *   - civil_society_privacy_advocates: organized beneficiary pushing broad interpretation
 *   - excluded journalists and researchers: speech/research interests not at the table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article17_erasure_right__privacy_fundamental_reading, 0.31).
domain_priors:suppression_score(article17_erasure_right__privacy_fundamental_reading, 0.18).
domain_priors:theater_ratio(article17_erasure_right__privacy_fundamental_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(article17_erasure_right__privacy_fundamental_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article17_erasure_right__privacy_fundamental_reading, rope).
narrative_ontology:human_readable(article17_erasure_right__privacy_fundamental_reading, "GDPR Article 17 Right to Erasure (Privacy Fundamental Reading)").
narrative_ontology:topic_domain(article17_erasure_right__privacy_fundamental_reading, "technology/legal/individual_rights").

domain_priors:requires_active_enforcement(article17_erasure_right__privacy_fundamental_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article17_erasure_right__privacy_fundamental_reading, '8ec014ec-a670-4a23-8401-29352edb9cbe').
narrative_ontology:cs_kernel_codification('8ec014ec-a670-4a23-8401-29352edb9cbe', formalized).
narrative_ontology:cs_authority_grounding('8ec014ec-a670-4a23-8401-29352edb9cbe', lineage).
narrative_ontology:cs_interpretation_layer_present('8ec014ec-a670-4a23-8401-29352edb9cbe').
narrative_ontology:cs_reading_relation('8ec014ec-a670-4a23-8401-29352edb9cbe', article17_erasure_right__competitive_moat_reading, influences).
narrative_ontology:cs_reading_relation('8ec014ec-a670-4a23-8401-29352edb9cbe', article17_erasure_right__censorship_mechanism_reading, coexists_with).
narrative_ontology:cs_axiom('8ec014ec-a670-4a23-8401-29352edb9cbe', foundational, individual_data_sovereignty_fundamental_right).
narrative_ontology:cs_axiom_status(individual_data_sovereignty_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('8ec014ec-a670-4a23-8401-29352edb9cbe', individual_data_sovereignty_fundamental_right, deontological).
narrative_ontology:cs_axiom('8ec014ec-a670-4a23-8401-29352edb9cbe', foundational, data_subject_power_rebalancing_justifies_compliance_cost).
narrative_ontology:cs_axiom_status(data_subject_power_rebalancing_justifies_compliance_cost, holdable).
narrative_ontology:cs_axiom_grounding('8ec014ec-a670-4a23-8401-29352edb9cbe', data_subject_power_rebalancing_justifies_compliance_cost, instrumental).
narrative_ontology:cs_reference_frame('8ec014ec-a670-4a23-8401-29352edb9cbe', individual_data_subject_empowerment).
narrative_ontology:cs_drift_state('8ec014ec-a670-4a23-8401-29352edb9cbe', contemporary_post_2018_gdpr_enforcement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8ec014ec-a670-4a23-8401-29352edb9cbe', '').
narrative_ontology:cs_kernel_id(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:constraint_beneficiary(article17_erasure_right__privacy_fundamental_reading, civil_society_privacy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(article17_erasure_right__privacy_fundamental_reading, online_platforms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals whose personal data is held by platforms and services. The right to erasure grants them the ability to request deletion of their data when it is no longer necessary for its original purpose, when they withdraw consent, or when it has been illegally processed. They gain back control over their digital footprint and can reduce ongoing surveillance and profiling. Exit from digital services is constrained, but the erasure right gives them leverage within those services.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_subjects, beneficiary,
    powerless, biographical, constrained, continental).

% Digital services and social networks that collect, store, and analyze user data as core business function. Article 17 requires them to delete personal data on verified request, destroying valuable datasets used for personalization, profiling, and targeted advertising. They bear the compliance cost of implementing deletion systems, maintaining audit trails, and managing requests at scale. Their data retention models are disrupted by mandatory erasure.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, online_platforms, payer,
    institutional, generational, mobile, global).

% Public authorities (DPAs) that enforce Article 17 and adjudicate disputes over erasure requests. They interpret the scope of the right, rule on when erasure is required or can be refused, and impose penalties for non-compliance. They set the practical boundary between individual sovereignty and institutional interests.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, data_protection_authorities, agenda_setter,
    institutional, generational, analytical, continental).

% Privacy-focused civil society organizations that litigate on behalf of data subjects, publish interpretive guidance on erasure scope, and lobby for broad readings of the right. They frame Article 17 as a fundamental rebalancing of power between individuals and data collectors. They extend the scope of erasure through strategic litigation.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, civil_society_privacy_advocates, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(article17_erasure_right__privacy_fundamental_reading, civil_society_privacy_advocates, agenda_setter).

% News organizations that would be compelled to delete archived reporting about public figures if erasure requests were applied broadly to all personal data. They claim that erasure could enable rewriting of public history and suppress legitimate journalism. They are not at the table during DPA rulings on erasure scope but have strong interests in how the right is bounded.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, journalistic_publishers, excluded,
    powerful, biographical, constrained, continental).

% Researchers studying platform behavior, content moderation, hate speech propagation, and algorithmic bias who rely on platform data access and historical datasets. Broad erasure requirements can fragment their datasets and prevent longitudinal research. They cannot negotiate directly with platforms or DPAs but are significantly affected by erasure policy.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, content_moderation_researchers, excluded,
    moderate, biographical, constrained, global).

% Non-EU platforms and data brokers operating across borders who face differential legal obligations. If Article 17 is narrowly applied, they retain competitive advantage over EU-compliant competitors; if broadly applied, the compliance burden redistributes competitive position. They have strong interests in how the right is scoped but are excluded from EU DPA proceedings.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, regulatory_arbitrage_actors, excluded,
    powerful, biographical, trapped, global).

% People who are the subjects of negative online information (past criminal records, leaked private details, harassment documentation, controversial statements made when younger). They could benefit from erasure of that data from search and platforms. However, they are not direct stakeholders in the Article 17 framework unless they are the data subject requesting erasure; journalists and public-interest advocates argue their erasure requests conflict with public memory and speech rights.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, individual_reputation_targets, excluded,
    powerless, biographical, trapped, continental).

% The European Parliament and Council that drafted GDPR and Article 17. They established the right as a fundamental entitlement within the EU legal order, balancing data protection with other rights. The legislative text is the kernel; its interpretation is contested across the three readings.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, eu_legislative_body, agenda_setter,
    institutional, generational, analytical, continental).

% International legal scholars and comparative-law observers tracking how different jurisdictions interpret deletion rights and individual data sovereignty. They analyze whether Article 17 represents a durable model or a transient governance experiment.
narrative_ontology:constraint_stakeholder(article17_erasure_right__privacy_fundamental_reading, observer_comparative_jurisprudence, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article17_erasure_right__privacy_fundamental_reading, data_subjects).
narrative_ontology:fixing_cost_class(article17_erasure_right__privacy_fundamental_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized, enforceable mechanism for individuals to reclaim control over their personal data by compelling deletion at source. Solves the collective-action problem of individual powerlessness against institutional data hoarding: without centralized deletion rights, individuals have no practical leverage to constrain profiling and retention.
% TRANSFER_FUNCTION: Transfers the cost of data retention from data subjects (who bear the surveillance and profiling cost, the loss-of-autonomy cost) to data controllers (who bear deletion, audit, and compliance infrastructure cost). Moves power to specify what data is kept from platforms to individuals.
% ABSENT_VOICES: Journalists and historical researchers argue they should be in the room but are excluded — their interests in preserving archived information and research datasets conflict with erasure requests, yet they are not primary parties to DPA proceedings. Content-moderation researchers and platform-external analysts have no formal voice in how the right is scoped. Regulatory arbitrage actors outside the EU cannot participate in defining the right's scope despite bearing the competitive consequences.
% DISAPPEARANCE_RATIONALE: If Article 17 and its enforcement vanished, platforms would retain all historical user data indefinitely unless explicitly deleted by users with technical knowledge. Individuals would lose their statutory deletion leverage; platforms would control indefinite profiling and retention as property. Personal data would accumulate permanently, surveillance infrastructure would intensify, and the leverage individuals possess under this reading would evaporate. The digital economy would reorganize around platform data possession as an irreversible asset.
% FOUNDING_PROBLEM: Digital platforms and data brokers collect and retain vast quantities of personal data on individuals, who have no practical way to reclaim control over or delete that data once collected. Individuals become permanently profiled subjects. The data is used for surveillance, discrimination, and manipulation. Platforms treat data as their permanent property and refuse deletion requests.
% FOUNDING_PROBLEM_CORROBORATION: Data protection authorities, civil society organizations, and individuals themselves attest the problem remains live — platforms continue aggressive data retention, dark patterns make opt-out difficult, and individuals report helplessness in reclaiming their data. Independent researchers document continued mission creep in data collection. Platform representatives dispute that the problem is as severe, but do not dispute that the underlying dynamic (data accumulation) exists. GDPR legislative history explicitly identifies data subject powerlessness as the problem the right was designed to address.
narrative_ontology:disappearance_verdict(article17_erasure_right__privacy_fundamental_reading, world_rearranges).
narrative_ontology:founding_problem_status(article17_erasure_right__privacy_fundamental_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article17_erasure_right__privacy_fundamental_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article17_erasure_right__privacy_fundamental_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article17_erasure_right__privacy_fundamental_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article17_erasure_right__privacy_fundamental_reading_tests).
:- end_tests(article17_erasure_right__privacy_fundamental_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.31) is moderate because the constraint transfers real value (control over data) from platforms to individuals, but platforms retain significant data possession and can delay/contest erasure requests. Suppression (0.18) is low because the constraint relies on individual requests and DPA adjudication rather than blanket bans; platforms can resist through litigation and technical obstruction, but suppression is not the primary enforcement mechanism. Theater (0.22) is modest: there is genuine deletion happening, but a growing share is compliance theater (deletion from search indexes while retaining in systems, technical delays, narrow interpretations of 'erasure' that platforms engineer). Accessibility_collapse (0.72) is high because once an individual understands they have deletion rights, the alternative (permanent profiling by data collectors) collapses — the right is fundamentally difficult to opt out of. Resistance (0.58) is substantial because platforms actively resist broad erasure interpretation through litigation, technical obstruction, and competing claims (speech preservation, data for research). The measurements show stability across the 10-year interval: extractiveness and suppression hover near their baseline, indicating neither intensification nor collapse of the constraint's operation — this is consistent with a rope in steady state, neither rapidly extracting nor integrating further. The shared time grid ensures every metric is authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   From the data_subjects seat (powerless, constrained exit), the constraint appears as genuine coordination solving powerlessness — they gain deletion leverage they otherwise lack, and the extraction from platforms is a justified correction for surveillance. From the online_platforms seat (institutional, mobile exit), the constraint appears as enforced asymmetric compliance cost — they must fund deletion infrastructure while users can request freely, creating a cost that has no reciprocal. From the civil_society seat (organized, mobile), the constraint is leverage for advancing the reading (pushing erasure scope broad). From the excluded journalists and researchers seats, the constraint appears as institutional power centralizing deletion decisions (via DPAs) at the cost of public memory and research. These divergences are structural: they arise from power asymmetry, exit options, and stake in the constraint's scope — not from observer-relative measurement. The engine computes d per-seat from beneficiary/victim + power + exit; the divergence is the feature the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Data_subjects are the primary beneficiaries: they are powerless, constrained to the digital ecosystem, yet gain deletion leverage. Their d is near 0.0 (full beneficiary) because the constraint subsidizes their agency relative to their structural position. Online_platforms are the primary payers: institutional power does not protect them from deletion cost; the constraint is enforced on them by DPAs; their exit is mobile (they can operate in non-GDPR jurisdictions) but only at significant cost. Their d is near 1.0 (full target) because the constraint extracts (compliance cost, data destruction) from their business model. Civil_society advocates are secondary beneficiaries (they collect legitimacy and influence from pushing the reading) with d near 0.2. DPAs are the administrative setter, with d near 0.5 (they administer but are not benefiting or paying in material sense). Journalists and researchers are excluded but would have high d toward the target if the reading extends erasure to historical data (their archives become deletion-subject). No directionality_overrides are needed; the structural derivation captures the seats accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (individual powerlessness to reclaim data) is LIVE according to this reading. Individuals still cannot efficiently delete data before Article 17; platforms still collect indefinitely; the power imbalance persists. The constraint's function is to correct that imbalance by giving individuals legal deletion leverage. Mandatrophy would emerge only if the founding problem died (if platforms voluntarily deleted on request, if surveillance infrastructure collapsed, if data lost value) — which has not happened. The constraint remains functionally responsive to its founding problem, so mandatrophy_resolved is false. However, the contested status of the founding problem (the competitive_moat_reading and censorship_mechanism_reading would dispute whether the problem is as stated) means the constraint's legitimacy is contested — not because it has outlived its function, but because the function itself is disputed. This is a live mandatrophy question, not a resolved one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    erasure_scope_boundary,
    'What constitutes ''personal data'' subject to erasure under Article 17? Does it include data aggregated with others? Data that has been anonymized? Data stored in backups? Data reconstructable from other sources?',
    'CJEU and national court rulings over time, establishing precedent on borderline cases. The scope boundary is the primary contested frontier.',
    'A narrow scope (erasure applies only to directly identifiable data in active systems) would limit the constraint''s reach, reducing extraction and platform cost. A broad scope (erasure extends to aggregates, reconstructable data, and backups) would increase extraction and platform burden. The classification of extraction hinges on where the scope boundary lands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(erasure_scope_boundary, empirical, 'What counts as erasable under Article 17.').

omega_variable(
    public_interest_exception_scope,
    'How broadly is the ''public interest exception'' to erasure interpreted? When can platforms refuse erasure on grounds of journalism, historical record, or research?',
    'Case law clarifying when Article 17(3)(a) exceptions apply — does it cover historical data subjects, archived journalism, research datasets?',
    'If the exception is broad, Article 17 has limited extraction effect on platforms protecting public-interest data. If narrow, platforms lose much of their defense against erasure requests. This directly modulates the constraint''s suppression and extraction metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_interest_exception_scope, empirical, 'Whether journalism and research carve out broad exceptions to erasure.').

omega_variable(
    individual_vs_collective_data_sovereignty,
    'Is Article 17''s framing of data sovereignty genuinely individual (a right belongs to the data subject alone to exercise), or does it implicitly establish collective dimensions (e.g., the right to not be profiled as a group, the right to data justice)?',
    'Interpretive and conceptual: does the civil society reading extend erasure to group protection? Does the competitive_moat_reading claim that the right creates collective incumbent protection?',
    'If data sovereignty is purely individual, erasure is granular and platform-friendly (delete my data, but you may keep others''). If collective, the right extends to refusing algorithmic profiling and group-based discrimination via deletion — which would substantially increase extraction and suppression.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individual_vs_collective_data_sovereignty, conceptual, 'Whether Article 17 is an individual or collective right.').

omega_variable(
    reading_contest_kernel_identity,
    'This reading frames Article 17 as an individual-sovereignty constraint. The sibling readings frame it as incumbent-protection (competitive_moat) or as censorship-enabling (censorship_mechanism). Are these three truly readings of a single kernel (GDPR Article 17 text), or do they address different kernels entirely?',
    'Structural analysis: if all three readings cite the same statutory text as their authority, they are readings of one kernel. If they cite different textual bases or ignore the text, they are different constraints. The GDPR Article 17 text is cited by all three; the contest is over what it means structurally (who benefits, what it enables). So they are readings of one kernel.',
    'If they are readings of one kernel, mandatrophy and contestation are properties of the kernel, not the reading. If they are different constraints, each has its own mandate. The classification approach changes: single kernel → reading-dependent axioms and relationality; multiple constraints → independent constraint classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_kernel_identity, conceptual, 'Whether the three readings share a kernel or address different constraints.').

omega_variable(
    epistemic_friction_in_requests,
    'How much epistemic friction (verification burden, documentation requirements, delays) do platforms impose on erasure requests? Does the reading''s premise of ''low friction'' match actual practice?',
    'Empirical audit: data subject organizations submit test requests and measure response times, denial rates, and compliance rates. DPA complaint data on request friction.',
    'High actual friction contradicts the reading''s premise of individual empowerment; it would suggest the constraint is more theater than leverage, shifting toward piton classification. Low friction supports the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_friction_in_requests, empirical, 'Whether erasure requests are genuinely low-friction or face hidden barriers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article17_erasure_right__privacy_fundamental_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(arti_tr_t0, observed).
narrative_ontology:measurement(arti_tr_t2, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 2, 0.19).
narrative_ontology:measurement_basis(arti_tr_t2, observed).
narrative_ontology:measurement(arti_tr_t4, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(arti_tr_t4, observed).
narrative_ontology:measurement(arti_tr_t6, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement_basis(arti_tr_t6, observed).
narrative_ontology:measurement(arti_tr_t8, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(arti_tr_t8, observed).
narrative_ontology:measurement(arti_tr_t10, article17_erasure_right__privacy_fundamental_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(arti_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(arti_be_t0, observed).
narrative_ontology:measurement(arti_be_t2, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 2, 0.29).
narrative_ontology:measurement_basis(arti_be_t2, observed).
narrative_ontology:measurement(arti_be_t4, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 4, 0.3).
narrative_ontology:measurement_basis(arti_be_t4, observed).
narrative_ontology:measurement(arti_be_t6, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement_basis(arti_be_t6, observed).
narrative_ontology:measurement(arti_be_t8, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement_basis(arti_be_t8, observed).
narrative_ontology:measurement(arti_be_t10, article17_erasure_right__privacy_fundamental_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(arti_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(arti_su_t0, observed).
narrative_ontology:measurement(arti_su_t2, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 2, 0.16).
narrative_ontology:measurement_basis(arti_su_t2, observed).
narrative_ontology:measurement(arti_su_t4, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 4, 0.17).
narrative_ontology:measurement_basis(arti_su_t4, observed).
narrative_ontology:measurement(arti_su_t6, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 6, 0.18).
narrative_ontology:measurement_basis(arti_su_t6, observed).
narrative_ontology:measurement(arti_su_t8, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 8, 0.18).
narrative_ontology:measurement_basis(arti_su_t8, observed).
narrative_ontology:measurement(arti_su_t10, article17_erasure_right__privacy_fundamental_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(arti_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article17_erasure_right__privacy_fundamental_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(article17_erasure_right__privacy_fundamental_reading, 0.12).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__competitive_moat_reading).
narrative_ontology:affects_constraint(article17_erasure_right__privacy_fundamental_reading, article17_erasure_right__censorship_mechanism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the article17_erasure_right kernel. The readings decompose on the contested question of what Article 17 structurally does: does it instantiate individual data sovereignty (this reading), function as incumbent protection via compliance cost asymmetry (competitive_moat), or enable strategic content suppression (censorship_mechanism)? Each reading has its own ε, beneficiary/victim structure, and classification. They are linked as a constraint family because they interpret the same statutory text differently. The network edges establish the family kinship and enable analysis of how contest over one reading affects the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
