% ============================================================================
% CONSTRAINT STORY: digital_money_emergence_boundary__conceptualization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_emergence_boundary__conceptualization_reading, []).

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
 *   constraint_id: digital_money_emergence_boundary__conceptualization_reading
 *   human_readable: Digital Money Emergence Boundary — Conceptualization Reading (Origin at Theoretical Thinkability)
 *   domain: monetary_economics/financial_history/technology_governance
 *
 * SUMMARY:
 *   This story models a historiographical convention: the practice of dating
 *   digital money's emergence to the moment it became theoretically thinkable
 *   — 1960s telecommunications feasibility advances culminating in the 1985
 *   blind-signature formalization. The convention performs genuine
 *   coordination: a field needs a periodization, and this one anchors
 *   curricula, citation lineages, and funding narratives while keeping
 *   'potential money' (uncirculated designs and prototypes) inside the object
 *   of study. The same structure allocates recognition asymmetrically:
 *   infrastructure builders (ATM networks 1967, clearing houses 1972,
 *   cross-border messaging 1977) are dated into prehistory, consumer-product
 *   teams are narrated as mere implementation, and monetary statisticians
 *   inherit the definitional strain of counting potential money in
 *   aggregates. Enforcement is active — peer review, textbook
 *   standardization, curriculum inertia — because two rival datings remain
 *   live. Per the epsilon-invariance principle this file covers ONLY the
 *   conceptualization reading; the infrastructure and consumer-holdings
 *   readings are separate constraints linked through
 *   network.affects_constraints. The claim/metrics independence rule applies:
 *   claimed_type records what I believe is structurally true; the metrics
 *   record what I believe descriptively true of the convention's operation;
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - academic_cryptography_community: primary beneficiary (organized/identity_locked) — collects citation priority and founding-narrative status
 *   - chaum_lineage_priority_holders: concentrated beneficiary (moderate/identity_locked) — named founders and patentees of the formalization era
 *   - monetary_history_gatekeepers: agenda setter (institutional/identity_locked) — editors, textbook authors, curriculum committees administering the periodization
 *   - payment_infrastructure_engineers: primary target (organized/constrained) — ATM/ACH/SWIFT-era builders dated into prehistory
 *   - electronic_money_product_implementers: secondary target (moderate/constrained) — e-purse-era teams classified as implementation
 *   - central_bank_monetary_statisticians: contingent target (institutional/constrained) — bear the M4/M5 definitional strain if the reading prevails
 *   - mutual_credit_system_designers: excluded voice (powerless/trapped) — digital mutual-credit systems outside every official historiography
 *   - international_payment_standards_bodies: analytical observer (institutional/analytical) — catalogs payment-system history without adjudicating origins
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, 0.52).
domain_priors:suppression_score(digital_money_emergence_boundary__conceptualization_reading, 0.38).
domain_priors:theater_ratio(digital_money_emergence_boundary__conceptualization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(digital_money_emergence_boundary__conceptualization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_emergence_boundary__conceptualization_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_emergence_boundary__conceptualization_reading, "Digital Money Emergence Boundary — Conceptualization Reading (Origin at Theoretical Thinkability)").
narrative_ontology:topic_domain(digital_money_emergence_boundary__conceptualization_reading, "monetary_economics/financial_history/technology_governance").

domain_priors:requires_active_enforcement(digital_money_emergence_boundary__conceptualization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_emergence_boundary__conceptualization_reading, '4dfc91d5-e66c-45e2-9049-36a0386aa898').
narrative_ontology:cs_kernel_codification('4dfc91d5-e66c-45e2-9049-36a0386aa898', distributed).
narrative_ontology:cs_authority_grounding('4dfc91d5-e66c-45e2-9049-36a0386aa898', distributed).
narrative_ontology:cs_reading_relation('4dfc91d5-e66c-45e2-9049-36a0386aa898', digital_money_emergence_boundary__infrastructure_reading, coexists_with).
narrative_ontology:cs_reading_relation('4dfc91d5-e66c-45e2-9049-36a0386aa898', digital_money_emergence_boundary__consumer_holdings_reading, influences).
narrative_ontology:cs_axiom('4dfc91d5-e66c-45e2-9049-36a0386aa898', foundational, theoretical_thinkability_constitutes_emergence).
narrative_ontology:cs_axiom_status(theoretical_thinkability_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('4dfc91d5-e66c-45e2-9049-36a0386aa898', theoretical_thinkability_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('4dfc91d5-e66c-45e2-9049-36a0386aa898', secondary, potential_money_counts_within_category).
narrative_ontology:cs_axiom_status(potential_money_counts_within_category, holdable).
narrative_ontology:cs_axiom_grounding('4dfc91d5-e66c-45e2-9049-36a0386aa898', potential_money_counts_within_category, conventional).
narrative_ontology:cs_reference_frame('4dfc91d5-e66c-45e2-9049-36a0386aa898', conceptual_precedence_origin).
narrative_ontology:cs_drift_state('4dfc91d5-e66c-45e2-9049-36a0386aa898', contemporary_cbdc_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4dfc91d5-e66c-45e2-9049-36a0386aa898', '').
narrative_ontology:cs_kernel_id(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, chaum_lineage_priority_holders).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, payment_infrastructure_engineers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, electronic_money_product_implementers).
narrative_ontology:constraint_victim(digital_money_emergence_boundary__conceptualization_reading, central_bank_monetary_statisticians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(digital_money_emergence_boundary__conceptualization_reading, monetary_history_gatekeepers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers in cryptography and applied mathematics who developed the formal tools — blind signatures, secure protocol designs, tamper-resistant token schemes — that make private digital cash thinkable. The periodization places their work at the origin of the field: survey articles open with their results, graduate curricula begin with their papers, and funding proposals cite a four-decade lineage running through them. Leaving the narrative would mean disowning the founding story their careers are built on.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community, beneficiary,
    organized, generational, identity_locked, global).

% The named individuals and firms of the formalization era — authors of the 1985 blind-signature paper, holders of the related patents, principals of the first digital-cash ventures. The dating convention concentrates founder status on them specifically: keynotes, retrospectives, and 'fathers of digital cash' features name them individually. Their public identity is fused with having been first in thought.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, chaum_lineage_priority_holders, beneficiary,
    moderate, biographical, identity_locked, global).

% Journal editors, textbook authors, and curriculum committees in monetary economics and the history of technology. They decide which origin story enters print, syllabi, and reference works, and they referee submissions whose framing depends on the chosen date. Their own courses and books are built on the periodization they administer, so revising it means revising themselves.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, monetary_history_gatekeepers, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(digital_money_emergence_boundary__conceptualization_reading, monetary_history_gatekeepers, beneficiary).

% The engineers and institutions that built interbank electronic transfer — automated teller networks from 1967, automated clearing houses from 1972, the cross-border messaging cooperative from 1977. Under the thinkability dating, their decades of moving real value are classified as prehistory: necessary background rather than emergence. Their record is fixed; no action available to them changes where the origin line falls.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, payment_infrastructure_engineers, payer,
    organized, generational, constrained, global).

% Product teams of the e-purse era — stored-value card ventures, early electronic-money issuers operating under the 2000 directive, wallet startups. The dating convention narrates their work as implementing ideas someone else already thought, which discounts their engineering, regulatory, and market-making achievements in histories and retrospectives.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, electronic_money_product_implementers, payer,
    moderate, biographical, constrained, global).

% Central-bank and statistical-agency staff responsible for monetary aggregates. If the thinkability dating governs the category, instruments that never circulated — research prototypes, patented designs, pilot tokens — fall inside 'digital money', forcing either awkward aggregate extensions that count potential money alongside circulating money or explicit exclusions that reopen the boundary question with every publication.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, central_bank_monetary_statisticians, payer,
    institutional, generational, constrained, global).

% Designers and operators of digitized mutual-credit and community-exchange schemes — local exchange trading systems and similar ledgers running digital units from the 1980s onward. Their systems moved digital value among real users years before commercial e-money, but they appear in neither the cryptography narrative nor the official payments history, and they have no seat in the periodization debate that decides whether they count.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, mutual_credit_system_designers, excluded,
    powerless, biographical, trapped, regional).

% Standards and oversight bodies that catalog payment-system history for interoperability and policy work. They compile timelines, take input from all the communities above, and publish neutral chronologies, but they adjudicate no origin claim and collect nothing from any dating.
narrative_ontology:constraint_stakeholder(digital_money_emergence_boundary__conceptualization_reading, international_payment_standards_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_emergence_boundary__conceptualization_reading, academic_cryptography_community).
narrative_ontology:fixing_cost_class(digital_money_emergence_boundary__conceptualization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes a single shared periodization for the history of digital money: one origin criterion — theoretical thinkability — around which curricula, citation lineages, funding narratives, and research programs in monetary economics, cryptography, and technology governance coordinate, and under which 'potential money' (designs and prototypes that never circulated) remains inside the object of study rather than discarded as prehistory.
% TRANSFER_FUNCTION: Moves recognition goods — citation priority, founding-narrative status, curriculum centrality, and the funding legitimacy that follows them — from the communities that built and deployed working payment infrastructure and consumer products to the community that formalized the underlying concepts; simultaneously moves the temporal boundary of the category 'digital money' backward over artifacts that never transacted value.
% ABSENT_VOICES: Mutual-credit and community-currency designers who operated digital instruments outside official rails from the 1980s onward are outside the conversation entirely — neither the cryptography narrative nor the official payments history counts their systems. The 1960s telecommunications engineers whose advances the reading conscripts as preconditions are mostly deceased and cannot contest how their work is framed. Users of failed e-money pilots, who held and lost digital value, have no seat in any academic periodization debate.
% DISAPPEARANCE_RATIONALE: Textbooks would lose their anchor chapter; citation lineages would redistribute (the 1985 formalization would become one technical milestone among several rather than the founding act); priority disputes between the cryptography, infrastructure, and consumer-payments communities would reopen; and research-funding narratives built on 'four decades of digital cash' would need reconstruction. The category boundary itself — whether uncirculated designs count as money — would revert to unresolved.
% FOUNDING_PROBLEM: As digital payment techniques proliferated across separate communities — telecommunications engineers, banking-network builders, academic cryptographers, e-purse ventures — the field lacked a determinate answer to 'when did digital money begin', which made teaching, citation, priority attribution, and funding-case construction unstable. The conceptualization reading resolved the attribution problem in favor of formalization: it fixed the origin at the moment the idea became rigorously thinkable (1960s telecommunications feasibility arguments culminating in the 1985 blind-signature formalization), giving the theoretical community founder status.
% FOUNDING_PROBLEM_CORROBORATION: Science-and-technology-studies scholarship on multiple discovery and priority allocation corroborates, from outside the benefiting parties, that origin conventions track disciplinary power as much as first articulation and that every active field requires some periodization convention. Payments-industry chroniclers and infrastructure historians — parties disadvantaged by this reading — independently attest that the periodization problem is real and ongoing while disputing this reading's answer, which is itself evidence the founding problem persists. No party denies the need for a convention; the contest is over which convention.
narrative_ontology:disappearance_verdict(digital_money_emergence_boundary__conceptualization_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_emergence_boundary__conceptualization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_emergence_boundary__conceptualization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_emergence_boundary__conceptualization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_emergence_boundary__conceptualization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_emergence_boundary__conceptualization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_emergence_boundary__conceptualization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: the convention's costs are recognition goods rather than material welfare, but recognition is a career-determining resource in the affected communities, and the asymmetry is systematic — it runs the same direction (toward formalization) across every venue the gatekeepers control. Suppression 0.38: gatekeeping pressure without prohibition — rival datings remain publishable and are in fact live (hence well below snare-level suppression); the mechanism is framing control, not exclusion from discourse. Theater ratio 0.30: anniversary conferences, founder retrospectives, and 'fathers of digital cash' hagiography are partly performative, but the periodization does real classificatory work in teaching and funding. Accessibility collapse 0.30: understanding the convention does not collapse alternatives — two sibling datings are visible and viable, so exits from the frame persist. Resistance 0.55: sustained contest from infrastructure historians and payments chroniclers, reopened periodically at each technology wave (e-purses, cryptocurrency, CBDCs). The measurement series are deliberately monotonic: the driver is consolidation (cryptocurrency-era canonization of the pre-Bitcoin digital-cash genealogy raised the value of priority claims; CBDC-era salience hardened gatekeeping), not oscillation, so no cyclical battery is warranted. All three tracked metrics share one six-point grid (1995–2025) so the engine samples a complete row at every point. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope. Fixing cost is authored 'cheap': nothing structural blocks revision — editorial coalitions re-date fields regularly — the binding obstacle is incumbent incentive, not fixability.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats should compute differently. From the gatekeeper and cryptography-community positions the convention is discovery: the moment the field finally understood what digital money IS, with the dating as a factual correction. From the infrastructure-engineer and implementer positions the same structure is closure: their record is fixed, their contributions are narrated as derivative, and no available action moves the origin line. The identity-lock mechanism is professional-identity fusion — careers, curricula, and self-concept constituted through the founding narrative — so the beneficiary seats cannot evaluate the dating from outside it without disowning their own trajectory; if that frame broke (for example, if a pluralist multi-origin account became standard), the beneficiary seats' classification would converge toward the payers'. The statistician seat is genuinely intermediate: its cost is conditional on the reading's adoption, so it experiences the convention as contingent exposure rather than settled loss.
 *
 * DIRECTIONALITY LOGIC:
 *   The cryptography community and the priority holders sit near the beneficiary end: the convention subsidizes them with recognition, and identity lock amplifies the subsidy by removing internal exit. The gatekeepers sit near-symmetric leaning beneficiary: they administer the convention and collect citation centrality through it. Infrastructure engineers and product implementers sit near the full-target end: they bear the recognition transfer with a fixed historical record and no exit from historiography. The statisticians sit mid-high: their burden materializes only if the reading prevails, damping their effective position below the engineers'. The excluded mutual-credit designers are structurally nearest full target — trapped outside the frame entirely, uncounted by any dating — but as an excluded voice they feed the consensus-provenance check, not the classification arithmetic. Scope is global for nearly every seat: the convention operates wherever monetary history is written, which raises verification difficulty and amplifies effective extraction for targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Mislabeling as pure coordination would miss that the same structure that periodizes also allocates credit asymmetrically and requires active enforcement against live rival datings — the coordination story is real but incomplete. Mislabeling as pure extraction would miss that the coordination function is genuine (fields do need periodizations), that the harmed parties suffer recognition losses rather than material harm, and that alternatives are suppressed only partially. On obsolescence: the founding problem — the need for a determinate periodization — is live, so no resolved-mandatrophy flag is declared. The forward risk is drift toward inertial maintenance: if a pluralist multi-origin consensus emerges, the thinkability dating could persist as anniversary ritual alone, with theater_ratio climbing past functional content; the theater series is the early-warning indicator to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the digital_money_emergence_boundary kernel — what would change structurally if a sibling reading (infrastructure or consumer holdings) were instantiated instead?',
    'Instantiate the sibling''s constraint file: its epsilon, beneficiary set (network operators or product vendors rather than theorists), and victim set (theorists demoted to prehistory) replace this story''s; compare computed classifications across the family.',
    'Under the infrastructure reading the discounted population flips — the formalization community becomes the party dated out of the origin; under the consumer-holdings reading both earlier communities subordinate to commercial deployers. Cross-reading comparison isolates how much of each classification is criterion-choice rather than world-structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this file is one of three rival datings; the disagreement is located in the category-membership criterion.').

omega_variable(
    category_membership_criterion,
    'Is membership in the category ''digital money'' determined by rigorous conceptual articulation, by operational value-transfer capability, or by direct user holdability?',
    'Philosophical analysis of money''s essence (function-based versus origin-based accounts) combined with disciplinary-convention study of how monetary economics textbooks and central-bank publications actually bound the category.',
    'Resolves which sibling reading is correct; determines which community legitimately collects priority and whether uncirculated designs belong inside monetary aggregates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_membership_criterion, conceptual, 'The kernel''s core contest: the criterion that fixes the emergence boundary.').

omega_variable(
    priority_vs_causal_contribution,
    'Does the theoretical-origin dating track actual causal contribution to subsequent digital money, or disciplinary power in the communities that write the histories?',
    'Citation-network and patent-lineage analysis quantifying the downstream influence of the 1985 formalization versus deployed transfer infrastructure (clearing volumes, cross-border messaging growth) and consumer products on today''s digital money arrangements.',
    'If credited priority systematically exceeds causal contribution, the convention''s asymmetric component is rent-like and its classification shifts snare-ward; if the two align, the coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priority_vs_causal_contribution, empirical, 'Whether the convention allocates recognition by contribution or by disciplinary power.').

omega_variable(
    cbdc_era_frame_survival,
    'Will the theoretical-thinkability origin survive the CBDC era, in which state-operated infrastructure dominates digital money''s frontier and infrastructure-first historiography regains salience?',
    'Track citation patterns and curriculum content in central-bank research and monetary economics syllabi across the CBDC deployment decade.',
    'If infrastructure framing regains dominance, this reading''s beneficiary structure atrophies and the convention drifts toward inertial maintenance by anniversary ritual alone; if theoretical framing holds, the hybrid coordination-plus-asymmetry structure persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cbdc_era_frame_survival, empirical, 'Persistence question: whether the reading''s frame survives the infrastructure-heavy CBDC era.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_emergence_boundary__conceptualization_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1995, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(digi_tr_t2001, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(digi_tr_t2007, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2007, 0.23).
narrative_ontology:measurement(digi_tr_t2013, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2013, 0.26).
narrative_ontology:measurement(digi_tr_t2019, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2019, 0.28).
narrative_ontology:measurement(digi_tr_t2025, digital_money_emergence_boundary__conceptualization_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(digi_be_t1995, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(digi_be_t2001, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2001, 0.32).
narrative_ontology:measurement(digi_be_t2007, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2007, 0.37).
narrative_ontology:measurement(digi_be_t2013, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2013, 0.42).
narrative_ontology:measurement(digi_be_t2019, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement(digi_be_t2025, digital_money_emergence_boundary__conceptualization_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1995, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 1995, 0.24).
narrative_ontology:measurement(digi_su_t2001, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2001, 0.27).
narrative_ontology:measurement(digi_su_t2007, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2007, 0.3).
narrative_ontology:measurement(digi_su_t2013, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2013, 0.33).
narrative_ontology:measurement(digi_su_t2019, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2019, 0.36).
narrative_ontology:measurement(digi_su_t2025, digital_money_emergence_boundary__conceptualization_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_emergence_boundary__conceptualization_reading, identity_coordination).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__infrastructure_reading).
narrative_ontology:affects_constraint(digital_money_emergence_boundary__conceptualization_reading, digital_money_emergence_boundary__consumer_holdings_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' decomposes under epsilon-invariance into three structurally distinct constraints: dating by theoretical thinkability (this file), by operational transfer infrastructure, and by direct consumer holdability. Each has a stable epsilon, its own beneficiary/victim structure, and its own enforcement profile; forcing one story to cover all three would make epsilon observer-dependent. Family links run through network.affects_constraints. The conceptualization reading sits upstream of the consumer-holdings reading (the formalization supplies the genealogy that commercial deployments cite) and coexists with the infrastructure reading (independent lineages with no shared premise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
