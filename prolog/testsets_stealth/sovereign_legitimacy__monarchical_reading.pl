% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Hereditary Sovereign Legitimacy — Monarchical Reading
 *   domain: political philosophy / constitutional theory / legitimacy studies
 *
 * SUMMARY:
 *   A hereditary monarchy claims and exercises supreme authority on the
 *   ground that legitimate power descends through bloodline, sanctified at
 *   accession by religious rite and armored by tradition. The arrangement
 *   solves a real problem — it fixes, before the fact, who commands — while
 *   channeling taxation, service, and obedience upward to the dynasty and its
 *   grantee hierarchy, and excluding the governed from any procedural role in
 *   authorizing any of it. Alternative accounts of legitimacy are not merely
 *   mistaken under this arrangement; they are criminal. This file
 *   instantiates the monarchical_reading of the sovereign_legitimacy kernel
 *   only (see kernel_context); the interval runs in years from a c. 1600
 *   baseline (t=0 is approximately 1600 CE, t=400 approximately 2000 CE),
 *   spanning the reading's ascendancy, contestation, and contraction. KEY
 *   AGENTS (by structural relationship): - hereditary_ruling_dynasty: Primary
 *   beneficiary and agenda-setter (institutional/identity_locked) — holds and
 *   transmits supreme authority; collects the arrangement's revenues -
 *   titled_aristocracy: Secondary beneficiary (powerful/constrained) —
 *   collects titles, estates, immunities, and offices -
 *   established_church_hierarchy: Beneficiary and co-administrator
 *   (institutional/constrained) — validates accession, collects establishment
 *   privileges - crown_officeholders: Beneficiary (organized/constrained) —
 *   careers and pensions flow through the patronage pyramid -
 *   common_subjects: Primary target (powerless/trapped) — bears taxation,
 *   conscription, and labor dues; no procedural voice -
 *   republican_dissidents: Excluded challenger (organized/trapped) — advances
 *   consent-based legitimacy claims under treason law - legitimacy_theorists:
 *   Analytical observer (analytical/analytical) — sees the full structure
 *   from outside
 *
 * KEY AGENTS:
 *   - hereditary_ruling_dynasty: Primary beneficiary and agenda-setter (institutional/identity_locked) — holds and transmits supreme authority by blood; collects the arrangement's revenues
 *   - titled_aristocracy: Secondary beneficiary (powerful/constrained) — collects lands, titles, tax immunities, and reserved offices
 *   - established_church_hierarchy: Beneficiary and co-administrator (institutional/constrained) — administers the sacral validation of accession; collects establishment privileges
 *   - crown_officeholders: Beneficiary (organized/constrained) — judges, governors, and household officers whose advancement runs through loyal service
 *   - common_subjects: Primary target (powerless/trapped) — bears taxation, conscription, and labor exactions with no authorizing voice
 *   - republican_dissidents: Excluded challenger (organized/trapped) — publishes and organizes for consent-based legitimacy under treason and sedition statutes
 *   - legitimacy_theorists: Analytical observer (analytical/analytical) — assesses the structure's claims from outside its offices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.68).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.6).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Hereditary Sovereign Legitimacy — Monarchical Reading").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political philosophy / constitutional theory / legitimacy studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'c75ad592-4171-4784-9b7b-4521d6bc2999').
narrative_ontology:cs_kernel_codification('c75ad592-4171-4784-9b7b-4521d6bc2999', formalized).
narrative_ontology:cs_authority_grounding('c75ad592-4171-4784-9b7b-4521d6bc2999', lineage).
narrative_ontology:cs_interpretation_layer_present('c75ad592-4171-4784-9b7b-4521d6bc2999').
narrative_ontology:cs_reading_relation('c75ad592-4171-4784-9b7b-4521d6bc2999', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('c75ad592-4171-4784-9b7b-4521d6bc2999', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('c75ad592-4171-4784-9b7b-4521d6bc2999', foundational, legitimate_authority_transmitted_by_inheritance_under_divine_sanction).
narrative_ontology:cs_axiom_status(legitimate_authority_transmitted_by_inheritance_under_divine_sanction, holdable).
narrative_ontology:cs_axiom_grounding('c75ad592-4171-4784-9b7b-4521d6bc2999', legitimate_authority_transmitted_by_inheritance_under_divine_sanction, theological).
narrative_ontology:cs_axiom('c75ad592-4171-4784-9b7b-4521d6bc2999', secondary, precommitted_bloodline_succession_is_necessary_for_political_order).
narrative_ontology:cs_axiom_status(precommitted_bloodline_succession_is_necessary_for_political_order, holdable).
narrative_ontology:cs_axiom_grounding('c75ad592-4171-4784-9b7b-4521d6bc2999', precommitted_bloodline_succession_is_necessary_for_political_order, instrumental).
narrative_ontology:cs_reference_frame('c75ad592-4171-4784-9b7b-4521d6bc2999', divine_right_hereditary_order).
narrative_ontology:cs_drift_state('c75ad592-4171-4784-9b7b-4521d6bc2999', contemporary_mass_politics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('c75ad592-4171-4784-9b7b-4521d6bc2999', '2026-08-05T12:00:00Z').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, titled_aristocracy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, established_church_hierarchy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, crown_officeholders).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, common_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, republican_dissidents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, common_subjects).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_right_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, primogeniture_succession_rule).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, hereditary_transmission_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds supreme executive, legislative, and judicial authority by right of birth, transmitted through the house's succession line. Confirms law, declares war, appoints ministers and judges, and controls the demesne revenues. Each reign is validated at accession by anointing and coronation oath administered by the established church. Members of the house cannot decline the role without breaking the succession claim itself; abdication is treated as rupture, not retirement.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty, agenda_setter,
    institutional, generational, identity_locked, national).

% Hold hereditary titles, landed estates, tax immunities, and reserved seats in the upper chamber and officer corps, all granted and confirmed by the crown. Their rank, marriage alliances, and legal privileges are denominated in the monarchical order; converting wealth into commerce does not carry rank with it. Military service and court loyalty are exchanged for continued privilege.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, titled_aristocracy, beneficiary,
    powerful, generational, constrained, national).

% Administers the anointing and coronation that validate each reign, preaches the duty of obedience, and holds established status, tithes, and seats in the upper chamber. Its liturgical calendar and doctrinal authority are woven into the state's ceremonies. Endorsing a rival account of where authority comes from would dissolve its own established position, so it co-administers the arrangement it benefits from.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, established_church_hierarchy, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, established_church_hierarchy, agenda_setter).

% Serve as judges, governors, ambassadors, and household officers at the crown's pleasure. Salaries, pensions, and honors flow through the patronage pyramid; advancement depends on demonstrated loyalty. Their careers have no equivalent ladder outside the court's service.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, crown_officeholders, beneficiary,
    organized, biographical, constrained, regional).

% Owe taxes, military service, and labor dues fixed by law and custom, without participating in any body that authorizes them. Compliance is owed from birth; there is no procedure by which they confer or withhold allegiance. They consume the order and continuity the arrangement provides, but cannot consent to or refuse its terms. Emigration is possible in principle yet costly, licensed, and sometimes barred; political exit does not exist.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, common_subjects, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, common_subjects, beneficiary).

% Publish, conspire, and organize for a rival account of where authority comes from — that it originates in the governed. Prosecuted under treason and sedition statutes; pamphleteers face censorship, organizers face prison or exile, and exile circles sustain the argument from abroad. Their exclusion from lawful political speech is maintained by the same statutes that protect the succession.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, republican_dissidents, excluded,
    organized, biographical, trapped, continental).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, republican_dissidents, payer).

% Assess the arrangement's claims from outside its offices — writing on the origin of authority, comparing regimes, and recording succession outcomes across borders and centuries. They hold no stake in the patronage pyramid and can say what participants cannot.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, legitimacy_theorists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_dynasty).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pre-committed, unambiguous answer to who holds supreme authority at every moment, including at succession, anchoring elite expectations and mass obedience without recurring negotiation; channels elite rivalry into court competition rather than open war; synchronizes law, war-making, and appointment under a single recognized head.
% TRANSFER_FUNCTION: Moves taxation, conscription, corvee labor, and fealty upward from subjects to the dynastic treasury and its grantee hierarchy (aristocracy, church, officeholders); moves legitimation downward from sacral ritual to the crown; moves offices, titles, and jurisdiction as inheritable or revocable grants along patronage lines.
% ABSENT_VOICES: Common subjects without any procedural voice, and republican dissidents criminalized under treason and sedition law, would object that authority's source was never theirs to confer; they stand outside the conversation by design — their exclusion is the enforcement object. Peasant and burgher assemblies that once negotiated obligations were dissolved or converted into ceremonial estates.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, succession would be immediately contested among pretenders, elites, and armies; the legitimacy vacuum would be filled by election, conquest, or written constitution; the church would lose its establishment anchor; aristocratic titles and immunities would void; and the patronage pyramid supporting the officeholding class would collapse into whatever new authority-allocating mechanism emerged.
% FOUNDING_PROBLEM: Early territorial states needed an unambiguous, continuously occupied locus of supreme authority: every interregnum invited elite war and peasant devastation. Hereditary right plus divine sanction answered 'who rules?' before the question could be asked — succession settled by birth, obedience anchored by sacral ritual, elite rivalry channeled into court competition rather than open warfare.
% FOUNDING_PROBLEM_CORROBORATION: Comparative-historical scholarship on state formation and succession violence — outside the benefiting parties — corroborates that the founding problem was real and that hereditary succession measurably reduced some classes of transition conflict. Whether the problem remains live is disputed: dynastic apologists and some political scientists attest its persistence wherever consent mechanisms fail or are absent, while republican theorists and constitutional historians attest that consent-based and legal-rational mechanisms now solve it at lower cost. No corroboration exists for the divine-sanction warrant itself outside the established church's own testimony.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the structure exhibits both a genuine coordination function — a pre-committed succession rule that removes the recurring question of who rules and channels elite rivalry into court competition — and asymmetric extraction: revenues, service, and decision-rights flow upward to the dynasty and its grantees while the governed hold no authorizing voice, all held in place by active enforcement (treason and sedition statutes, licensing, religious conformity). Metrics describe the arrangement's operation at interval end: extractiveness 0.68 (high but not total — the order and succession-clarity functions are real and consumed by every seat); suppression 0.60 (enforcement machinery real but past its seventeenth–eighteenth-century peak); theater_ratio 0.62 (above the Goodhart marker because the aggregate blends strongly enforced strongholds with residual instances where ceremony carries most of the legitimating load — see the ceremonial_vestige_mislabeling omega); accessibility_collapse 0.55 (rival legitimacy accounts remained thinkable, publishable abroad, and eventually victorious across much of the world — alternatives never fully collapsed); resistance 0.72 (civil war, revolution, and sustained dissident networks across the interval). All three tracked series run on one shared six-point grid (t = 0, 80, 160, 240, 320, 400). Extraction deepened through the fiscal-military absolutist era (0.62 to 0.74), then eased slightly as dynasties ceded fiscal control to representative bodies while retaining core revenues in their strongholds. suppression_requirement is authored because enforcement capacity is the traced dynamic here: it built to a peak (0.78 at t=160 — licensing acts, seditious-libel prosecutions, conformity tests) and then decayed (0.60) as rivals were accommodated, defeated, or driven into exile. Theater rose monotonically as ritual absorbed a growing share of the legitimating work. Succession crises punctuate the record episodically (local spikes in resistance and enforcement around contested accessions), but the grid samples the secular trend rather than the episodic spikes; the oscillation driver is demographic accident in succession lines, not intermittent reinforcement. Suppression composes structural machinery (statutes, licensing, conformity tests) with internalized deference (sacral awe, habituated obedience); the scalar cannot separate the two, so the composition is routed to the deference_internalization_share omega. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and national scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   From the throne's seat the arrangement presents as the natural shape of the cosmos — authority descending as light from the sun, with no alternative even grammatically available; a seat computing there should return something mountain-like. From the subject's seat the same structure is a one-way flow of obligations with no authorizing consent and no exit; a seat computing there should return something heavily extractive. From the aristocrat's and churchman's seats it is earned-and-owed privilege, experienced as fair exchange of service for station. The theorist's seat sees a contingent construction with a measurable founding problem. The engine computes these divergences from the structural data; the gap between the throne's mountain-like experience and the subject's extraction experience is the perspectival content this story exists to preserve.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d: the dynasty sits nearest the beneficiary pole (it writes the rules, collects the revenues, and is locked into its own subsidy); titled_aristocracy, established_church_hierarchy, and crown_officeholders sit nearby, collecting granted shares, with constrained exit keeping them invested in the arrangement's continuation. Victim declarations drive high d: common_subjects sit near the full-target pole, amplified by trapped exit (no procedural voice, costly and licensed emigration); republican_dissidents sit at the target pole through direct exposure to the enforcement apparatus itself. National spatial scope modestly amplifies effective extraction for targets by raising the verification cost of abuses at distance — the distant province sees the tax collector, not the court. No directionality_overrides are authored: the derivation from beneficiary/victim declarations plus exit options reproduces the structural relationships without correction. Identity-lock note: the dynasty's exit is identity_locked through institutional fusion — the house has become the crown; a renouncing heir does not step outside the arrangement so much as trigger a succession crisis inside it. If that identity frame broke (a dynasty voluntarily dissolving its own claim), the arrangement would lose its holder-class abruptly rather than decaying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an unambiguous, continuously occupied locus of supreme authority — is contested rather than dead: comparative-historical scholarship corroborates that it was real and that hereditary succession reduced some classes of transition violence, while consent-based mechanisms now solve it at lower cost in most polities. Where the reading still substantively governs, the function persists and mandatrophy is not resolved; where only ceremony remains, the mandate is dead and the instance drifts toward theatrical maintenance — visible in the monotonic theater_ratio rise to 0.62. Classifying the structure as tangled_rope prevents the two symmetrical mislabels: a pure-snare reading would erase the real succession-coordination achievement that even hostile historians corroborate; a pure-rope reading would erase the exclusion rents and criminalized alternatives that the victim declarations record. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges, which raises no zombie flag — correctly, since the arrangement's persistence tracks a disputed-but-live problem, not a dead one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_structural_delta,
    'This constraint is one reading of the sovereign_legitimacy kernel (reading: monarchical_reading). What structurally changes if a sibling reading is adopted instead?',
    'Adoption of republican_reading relocates legitimacy''s source from inheritance to consent: the dynasty converts from agenda-setter to abolished office or symbolic residue, common_subjects convert from victims to citizen-principals, and epsilon falls toward the coordination floor. Adoption of constitutional_hybrid_reading splits authority by function, leaving the dynasty a bounded ceremonial beneficiary with political authority delegated onward.',
    'Sibling adoption changes the victim set, the beneficiary set, and the effective-extraction profile wholesale; classifications computed on this file do not transfer to sibling files, and cross-reading comparison must run per-file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Committer-frame routing: what each sibling reading of the sovereign_legitimacy kernel would change structurally relative to this monarchical reading.').

omega_variable(
    divine_sanction_coordination_effect,
    'Does divine sanction carry observable coordination effects independent of individual belief, or is the sacral layer purely conventional decoration on coercion?',
    'Compare legitimacy durability under succession stress across regimes matched on coercion capacity but differing on sacral validation depth; if sacral regimes survive equivalent shocks with less enforcement, the sanction layer does independent coordinating work.',
    'If purely conventional, the arrangement''s persistence reduces to enforcement plus inertia and the snare-leaning reading strengthens; if belief-independent coordination effects exist, part of the measured stability is genuine rope-function and the tangled_rope classification firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_sanction_coordination_effect, empirical, 'Whether the theological validation layer contributes coordination value beyond coercion.').

omega_variable(
    succession_stability_net_advantage,
    'Does hereditary succession actually reduce succession conflict relative to consent-based selection, net of wars triggered by disputed lines, royal minorities, and dynastic extinction?',
    'Comparative dataset of transition episodes per century across hereditary, elective, and consent-based regimes, coding contested-succession frequency and severity; the founding-problem corroboration record supplies the baseline.',
    'If the net advantage is illusory, the coordination half of the tangled_rope claim weakens toward snare (coordination story as cover); if robust, the hybrid classification holds and part of the measured extraction is the price of the order function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_stability_net_advantage, empirical, 'Net coordination advantage of bloodline succession over consent-based selection.').

omega_variable(
    deference_internalization_share,
    'How much of subject compliance with the hereditary order is structural (treason statutes, censorship, religious tests) versus internalized (deference norms, sacral awe, habituated obedience)?',
    'Post-abolition compliance trajectories: if deferential behavior and legitimacy sentiment persist after the enforcement machinery is removed, the internalized share is large; rapid reversion to rival legitimacy claims indicates the structural share dominated.',
    'If largely internalized, effective suppression exceeds the structural measure and outlives the regime that installed it, predicting post-transition instability and inflating the constraint''s durable footprint; if largely structural, abolition releases compliance quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_internalization_share, empirical, 'Structural versus internalized composition of the arrangement''s suppression of alternative legitimacy claims.').

omega_variable(
    ceremonial_vestige_mislabeling,
    'In surviving instances, is the operating arrangement still this monarchical reading (substantive inherited authority) or has it silently become the constitutional_hybrid_reading while retaining monarchical vocabulary?',
    'Test whether the crown retains any discretionary act that binds without elected-body consent: if none exists, the surviving instance is governed by the hybrid reading and this file''s end-state metrics describe vestige wearing the old name.',
    'If vestige, the end-state extractiveness and suppression attributed here belong to the sibling''s file and this reading''s true current domain shrinks to the strongholds where discretion survives; epsilon-invariance requires the decomposition rather than one averaged story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceremonial_vestige_mislabeling, conceptual, 'Whether residual monarchical instances are this reading or the hybrid sibling operating under monarchical labels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sove_tr_t80, sovereign_legitimacy__monarchical_reading, theater_ratio, 80, 0.35).
narrative_ontology:measurement(sove_tr_t160, sovereign_legitimacy__monarchical_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement(sove_tr_t240, sovereign_legitimacy__monarchical_reading, theater_ratio, 240, 0.48).
narrative_ontology:measurement(sove_tr_t320, sovereign_legitimacy__monarchical_reading, theater_ratio, 320, 0.55).
narrative_ontology:measurement(sove_tr_t400, sovereign_legitimacy__monarchical_reading, theater_ratio, 400, 0.62).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(sove_be_t80, sovereign_legitimacy__monarchical_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(sove_be_t160, sovereign_legitimacy__monarchical_reading, base_extractiveness, 160, 0.74).
narrative_ontology:measurement(sove_be_t240, sovereign_legitimacy__monarchical_reading, base_extractiveness, 240, 0.72).
narrative_ontology:measurement(sove_be_t320, sovereign_legitimacy__monarchical_reading, base_extractiveness, 320, 0.7).
narrative_ontology:measurement(sove_be_t400, sovereign_legitimacy__monarchical_reading, base_extractiveness, 400, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sove_su_t80, sovereign_legitimacy__monarchical_reading, suppression_requirement, 80, 0.72).
narrative_ontology:measurement(sove_su_t160, sovereign_legitimacy__monarchical_reading, suppression_requirement, 160, 0.78).
narrative_ontology:measurement(sove_su_t240, sovereign_legitimacy__monarchical_reading, suppression_requirement, 240, 0.74).
narrative_ontology:measurement(sove_su_t320, sovereign_legitimacy__monarchical_reading, suppression_requirement, 320, 0.66).
narrative_ontology:measurement(sove_su_t400, sovereign_legitimacy__monarchical_reading, suppression_requirement, 400, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'sovereign legitimacy' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel: monarchical (this file — legitimacy by downward transmission; dynasty and aristocracy as beneficiaries, subjects as victims, high suppression of alternatives), republican_reading (legitimacy by upward consent; the citizenry as principal, a different victim/beneficiary structure and epsilon), and constitutional_hybrid_reading (dual-sourced legitimacy; partitioned victim and beneficiary sets). Each is authored as its own story with its own stable epsilon; this file links its siblings here. Direction of influence: this reading's crisis episodes (wars of succession, absolutist overreach) created the structural conditions under which the sibling readings gained adoption ground — an influences-edge recorded in cs_structure.reading_relations toward the hybrid, whose very existence as a coherent mediating framework is why the two pure readings coexist rather than foreclose each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
