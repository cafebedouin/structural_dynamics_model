% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__expansive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Minority Protections (Expansive Reading): Functional Continuity Guarantee for Pre-1923 Religious Governance
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the lausanne_minority_protections
 *   kernel: the expansive_reading, under which the 1923 Treaty of Lausanne
 *   guarantees functional continuity of pre-1923 religious governance -
 *   corporate self-administration, property rights, and clergy formation
 *   through theological schools - for the recognized non-Muslim institutions
 *   of Turkey. The epsilon referent is the standing arrangement under
 *   contest, the guarantee as this reading holds it, assessed by this
 *   reading's own lights: an obligor state administering a protective frame
 *   around four institutional seats that cannot relocate their sacred centers
 *   or their endowed assets. The claimed type and the metrics are authored
 *   independently: the claim is rope (a genuine coordination arrangement
 *   solving the post-imperial coexistence problem), while the metrics
 *   describe moderate extraction with a documented erosion-era drift and
 *   partial recovery - the engine measures any divergence. KEY AGENTS (by
 *   structural relationship): ecumenical_patriarchate - primary beneficiary
 *   (organized/identity_locked), a global See legally anchored in Istanbul;
 *   armenian_patriarchate_of_constantinople and office_of_chief_rabbinate -
 *   secondary beneficiaries (organized/constrained);
 *   minority_vakif_foundations - property-holding beneficiary
 *   (moderate/trapped); turkish_republic_state_authorities - obligor and
 *   simultaneous administrator (institutional/arbitrage);
 *   european_court_of_human_rights - adjudicative observer
 *   (institutional/analytical); departed_greek_community_members - excluded
 *   voice (powerless/trapped).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.32).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.5).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Minority Protections (Expansive Reading): Functional Continuity Guarantee for Pre-1923 Religious Governance").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '50bf2f66-ad33-40e6-81af-e88c082e2ec2').
narrative_ontology:cs_kernel_codification('50bf2f66-ad33-40e6-81af-e88c082e2ec2', fixed_text).
narrative_ontology:cs_authority_grounding('50bf2f66-ad33-40e6-81af-e88c082e2ec2', lineage).
narrative_ontology:cs_interpretation_layer_present('50bf2f66-ad33-40e6-81af-e88c082e2ec2').
narrative_ontology:cs_reading_relation('50bf2f66-ad33-40e6-81af-e88c082e2ec2', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('50bf2f66-ad33-40e6-81af-e88c082e2ec2', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('50bf2f66-ad33-40e6-81af-e88c082e2ec2', foundational, institutional_continuity_treaty_guaranteed).
narrative_ontology:cs_axiom_status(institutional_continuity_treaty_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('50bf2f66-ad33-40e6-81af-e88c082e2ec2', institutional_continuity_treaty_guaranteed, conventional).
narrative_ontology:cs_axiom('50bf2f66-ad33-40e6-81af-e88c082e2ec2', secondary, treaty_obligations_prevail_over_domestic_statute).
narrative_ontology:cs_axiom_status(treaty_obligations_prevail_over_domestic_statute, holdable).
narrative_ontology:cs_axiom_grounding('50bf2f66-ad33-40e6-81af-e88c082e2ec2', treaty_obligations_prevail_over_domestic_statute, conventional).
narrative_ontology:cs_reference_frame('50bf2f66-ad33-40e6-81af-e88c082e2ec2', functional_continuity_of_pre_1923_governance).
narrative_ontology:cs_drift_state('50bf2f66-ad33-40e6-81af-e88c082e2ec2', contemporary_centenary_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('50bf2f66-ad33-40e6-81af-e88c082e2ec2', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, ecumenical_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_patriarchate_of_constantinople).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, office_of_chief_rabbinate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, minority_vakif_foundations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(lausanne_minority_protections__expansive_reading, turkish_republic_state_authorities).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, functional_continuity_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, treaty_fundamental_law_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads the Eastern Orthodox communion worldwide from Istanbul, where its cathedral, offices, and printing house sit under Turkish jurisdiction. It depends on the treaty guarantee for its legal personality, its right to consecrate bishops, and its schools; the state has withheld permission to reopen its theological seminary on Heybeli Island since 1971, blocking clergy formation. Relocating the See would dissolve the office's meaning - its authority is bound to the city by canon and fifteen centuries of continuity - so it petitions, litigates at the margins, and cultivates diaspora support rather than moving.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, ecumenical_patriarchate, beneficiary,
    organized, civilizational, identity_locked, global).

% Administers the Armenian Apostolic community's churches, schools, and charitable foundations in Turkey. Its patriarchs are elected under procedures the state must permit; its foundations' property titles have been contested in the courts for decades. The community it serves has shrunk through emigration, thinning its base, but its institutions remain the center of communal life. Leaving Turkey would mean abandoning the remaining community; staying means working through administrative channels whose decisions it does not control.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_patriarchate_of_constantinople, beneficiary,
    organized, generational, constrained, national).

% Represents Turkey's Jewish community, overseeing synagogues, the communal school system, and charitable foundations. The community is small and aging; marriages, burials, dietary certification, and schooling all run through offices whose legal footing rests on the treaty-era settlement. It maintains quiet working relations with state authorities and avoids public controversy, since its operational permissions can be narrowed informally at any time.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, office_of_chief_rabbinate, beneficiary,
    organized, generational, constrained, national).

% Hold the communities' accumulated wealth - schools, hospitals, orphanages, immoveable property - in endowment form. Their boards are elderly and hard to refill; title to hundreds of properties was thrown into doubt by a 1974 court ruling, with partial restorations following EU-linked reforms in the 2000s and 2010s. Their assets cannot leave the country, so their survival turns entirely on favorable administrative interpretation of the property articles.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, minority_vakif_foundations, beneficiary,
    moderate, biographical, trapped, national).

% Implements the treaty settlement through its own ministries, governorships, and courts: it decides which foundation elections to permit, which properties to register, which schools to open. It bears the costs of restraint - foregone control over religious corporations, a property regime carved out of general law, international scrutiny of its administration - while collecting the recognition and alliance benefits that compliance purchases. It can widen or narrow day-to-day practice by administrative choice without ever formally repudiating anything, and domestic nationalist constituencies punish visible generosity toward minority institutions.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_republic_state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__expansive_reading, turkish_republic_state_authorities, payer).

% Adjudicates individual applications from members of the protected communities, usually framed as property, education, or association claims rather than treaty articles directly. Its judgments create precedents both sides cite; its remedies are slow and indirect, but it is the only forum where community members can challenge state practice above the domestic level.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, continental).

% Left Istanbul in waves after the 1955 pogrom and the 1964 expulsion of Greek citizens; many lost nationality and property. They hold the strongest living memory of what the guarantee covered and the sharpest objections to how it was narrowed, but they stand outside every forum where the arrangement's meaning is now decided - their claims surface only through archives, diaspora associations, and occasional litigation by heirs.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, departed_greek_community_members, excluded,
    powerless, biographical, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared legal frame under which a new nation-state and embedded pre-modern religious corporations coexist: it fixes which institutions continue, who may run them, who may train their clergy, and on what property basis - questions that would otherwise be settled unilaterally, and repeatedly, by state discretion.
% TRANSFER_FUNCTION: Moves legal control and forbearance from the state to the recognized communities' institutions - self-administration, property administration, clerical training - and moves civic allegiance and the international-recognition yield of compliance from the communities to the state. Concretely: foundation deeds, school permits, election permissions, and registered titles.
% ABSENT_VOICES: The departed Greek community members (1955 pogrom survivors, 1964 deportees) hold the sharpest objections and have no standing anywhere the arrangement's meaning is decided; the Muslim minority of Western Thrace, the mirror beneficiaries of the reciprocal articles, sits outside this frame entirely in Greek domestic politics; secularist Turkish constituencies who object to any institutional religious privilege speak only through state organs; and the drafting generation whose intent both sides invoke is dead. Their objections enter only through archives, diaspora advocacy, and scholarship.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would strip the recognized communities' institutions of their legal foundation: endowment titles revert to contested general-law status, patriarchal and rabbinical elections lose their procedural basis, the remaining schools close for want of a permitting frame. Greece would invoke the reciprocal articles, Strasbourg applications would multiply, and the republic's recognition-era bargain would be publicly repudiated - a full rearrangement of the legal position of every seated party.
% FOUNDING_PROBLEM: The post-Ottoman settlement had to determine the status of the religious minorities remaining after the population exchanges and secure great-power recognition of the new Turkish republic; Lausanne traded codified minority protections for sovereignty recognition, embedding the surviving non-Muslim institutions of Istanbul in the treaty itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Greek government's standing diplomacy invokes the reciprocal articles; the European Court of Human Rights and the EU Commission's progress reports treat the minority-institution questions as unresolved treaty matters; Turkish official statements themselves invoke Lausanne when defending their practice, attesting the framework's continuing force; and the international-law literature on the Lausanne minority regime documents the settlement bargain. No corroborating source outside the beneficiary set treats the founding problem as resolved.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__expansive_reading_tests).
:- end_tests(lausanne_minority_protections__expansive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.32: the arrangement's net take is modest - the state's restraint costs are real but were the entry price of the international recognition it sought in 1923, and the minority seats pay dependence and litigation costs while receiving the frame that keeps their institutions lawful at all. Suppression 0.50: the guarantee holds through reputational, adjudicative, and diplomatic pressure rather than physical coercion; alternatives (general associations law, bilateral arrangements) remain partly available, so alternatives are neither fully collapsed nor fully live. Theater_ratio 0.33: most of the arrangement's activity is functional (worship, foundation elections, partial restitutions), but a persistent slice is ceremonial invocation of the treaty while named gaps - the closed seminary, contested titles - stay shut. Accessibility_collapse 0.48 and resistance 0.62: the principal obligor has actively resisted the expansive scope for a century (school closures, title challenges, election obstruction), which is why the reading survives by argument rather than by settled administration. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. The temporal series run on one shared grid (1923, 1942, 1964, 1971, 2008, 2023) with every tracked metric authored at every point. The cycle is driven by external anchoring rather than intermittent reinforcement: great-power oversight and post-settlement goodwill (low burden), Cold War-era indifference letting administrative erosion accumulate (rising burden, peaking around the 1971 seminary closure), then EU-conditionality leverage forcing partial restoration (falling burden). The scalar base_properties values are the interval-end (2023) column. On suppression mechanism: the arrangement's coercive edge is predominantly structural (administrative discretion, title insecurity, permitting power) with a modest internalized layer - generations of communal self-censorship and learned quietism that persist where no barrier currently binds.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different arrangements from the same structure. From the obligor-administrator seat, the guarantee looks like a sovereignty concession it honors selectively and administers itself - a burden it can lighten by administrative choice. From the beneficiary seats, the same structure looks like a lifeline whose value depends on winning an interpretive contest they do not control; the Patriarchate seat additionally carries identity lock (its canonical existence is fused with its Istanbul location), so its computed position sits deeper in dependence than its organizational resources suggest. The three same-standing minority institutions diverge laterally: the Patriarchate is globally prominent but locally identity-locked, the Rabbinate survives by deliberate quietism, and the foundations are asset-rich but exit-trapped - equal nominal standing, differentiated exits. The Strasbourg seat sees a century-long scope dispute rather than either a burden or a lifeline. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The four beneficiary declarations place the minority institutional seats near the beneficiary end (low d, damped or inverted effective extraction): the arrangement subsidizes their continued legal existence. The obligor state is declared payer, but a derivation from the payer role alone would push its d toward the full-target end and overshoot: the state simultaneously collects the arrangement's legitimacy yield, controls its day-to-day administration, and arbitrages between compliance postures without formal repudiation. The directionality override sets the institutional power atom to d=0.58 - slightly target-side of symmetric - capturing that dual position; only one institutional-power agent exists in this story, so the atom-keyed override is precise. The observer seat is analytical and feeds no extraction arithmetic; the excluded seat is commentary-grade and drives no classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - how the republic houses its remaining non-Muslim religious corporations - is still live at greatly reduced demographic scale, and the disappearance verdict is world_rearranges, so the status-by-verdict pair shows no dead-mandate mismatch and no zombie flag. Theater peaked at 0.52 around the 1971 seminary closure and receded to 0.33 after the EU-era restitutions: the arrangement is not maintained chiefly by performance, because the beneficiary seats still actively defend it (litigation, petitions, diaspora mobilization) and the obligor still pays real restraint costs. The classification guards against two opposite errors: reading the state's compliance costs as extraction from it (they are the price of the recognition bargain it entered) and reading the minority seats' dependence as consent (it is enforced by their lack of alternatives - identity lock for the See, asset trap for the foundations). Mandatrophy resolution therefore holds the arrangement in its coordinative form while the omegas track the two ways it could degrade: scope-narrowing (functional_continuity_scope) and demographic hollowing (demographic_viability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the lausanne_minority_protections kernel; what happens to the arrangement''s structure if the restrictive_reading prevails in the interpretive contest?',
    'Track authoritative interpretation over time: Turkish Constitutional Court and Council of State doctrine, European Court of Human Rights admissibility and merits holdings on Lausanne-scoped claims, and treaty-body practice. A settled judicial consensus on the scope of Articles 37-45 resolves the contest.',
    'If the restrictive reading prevails, institutional self-administration, property, and clergy-formation guarantees drop out of the arrangement entirely: the beneficiary seats lose their protective cover and the structure collapses toward bare domestic discretion. If the expansive reading consolidates, the coordination structure stabilizes at its authored profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Survival of the expansive reading against its siblings determines whether this constraint exists at all.').

omega_variable(
    functional_continuity_scope,
    'Does ''functional continuity of pre-1923 religious governance'' require enabling the institutions'' current functions (training new clergy, replacing leadership, acquiring property) or only tolerating whatever pre-1923 functions happen to survive?',
    'Travaux preparatoires analysis, comparison with the parallel articles binding Greece toward its Muslim minority, and adjudication of seminary and leadership-election claims in Strasbourg and domestic courts.',
    'A forward-looking scope keeps the guarantee functionally complete and the arrangement genuinely coordinative; a backward-looking scope converts it into a museum provision, driving theater upward and pushing the beneficiary seats toward inertial dependence regardless of the reading''s formal victory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_continuity_scope, conceptual, 'Whether the guarantee''s scope is dynamic (covering living institutional needs) or static (preserving survivals).').

omega_variable(
    reciprocity_load_bearing,
    'Is the arrangement''s stability dependent on reciprocity with the mirror obligations owed to the Muslim minority of Western Thrace, or does it stand independently?',
    'Compare compliance trajectories in both states across crisis periods (Cyprus disputes, Aegean tensions): correlated retaliatory narrowing indicates reciprocity is load-bearing; independent trajectories indicate it is not.',
    'If load-bearing, the arrangement is partly an interstate coordination device and its durability exceeds what domestic interpretation alone would predict; if independent, its fate rides wholly on the domestic interpretive contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_load_bearing, empirical, 'Whether reciprocal mirror obligations anchor the guarantee''s stability.').

omega_variable(
    demographic_viability,
    'Can institutional continuity remain functionally real at the protected communities'' current scale (orders of magnitude smaller than in 1923), or has the guarantee become primarily symbolic?',
    'Longitudinal institutional-capacity data: clergy ordination rates, school enrollments, foundation board vacancies, actively administered property counts.',
    'If symbolic, performative maintenance rises structurally and the arrangement drifts toward ceremonial persistence even if the expansive reading wins every interpretive battle; if viable, the coordinative character holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_viability, empirical, 'Whether shrunken demographics can still sustain living institutions under the guarantee.').

omega_variable(
    self_implementation_paradox,
    'The state that bears the obligation also administers it: does the arrangement survive hostile domestic administration on its own terms, or does it function only under external anchoring (EU conditionality, Strasbourg exposure, guarantor attention)?',
    'Compare protection outcomes across periods of strong versus weak external anchoring (EU candidacy era circa 1999-2005 versus the post-candidacy stall) holding the legal text constant.',
    'If external anchoring is necessary, the arrangement''s persistence is contingent on forces outside this reading''s own enforcement theory, and any stability assessment must discount accordingly; if not, the domestic incorporation is self-sustaining.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_implementation_paradox, empirical, 'Whether the obligor-administered arrangement is robust or externally propped.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 1923, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_expansive_tr_t1923, lausanne_minority_protections__expansive_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t1923, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t1942, lausanne_minority_protections__expansive_reading, theater_ratio, 1942, 0.16).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t1942, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t1964, lausanne_minority_protections__expansive_reading, theater_ratio, 1964, 0.34).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t1964, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t1971, lausanne_minority_protections__expansive_reading, theater_ratio, 1971, 0.52).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t1971, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t2008, lausanne_minority_protections__expansive_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t2008, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t2023, lausanne_minority_protections__expansive_reading, theater_ratio, 2023, 0.33).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(lausanne_expansive_be_t1923, lausanne_minority_protections__expansive_reading, base_extractiveness, 1923, 0.22).
narrative_ontology:measurement_basis(lausanne_expansive_be_t1923, observed).
narrative_ontology:measurement(lausanne_expansive_be_t1942, lausanne_minority_protections__expansive_reading, base_extractiveness, 1942, 0.27).
narrative_ontology:measurement_basis(lausanne_expansive_be_t1942, observed).
narrative_ontology:measurement(lausanne_expansive_be_t1964, lausanne_minority_protections__expansive_reading, base_extractiveness, 1964, 0.36).
narrative_ontology:measurement_basis(lausanne_expansive_be_t1964, observed).
narrative_ontology:measurement(lausanne_expansive_be_t1971, lausanne_minority_protections__expansive_reading, base_extractiveness, 1971, 0.44).
narrative_ontology:measurement_basis(lausanne_expansive_be_t1971, observed).
narrative_ontology:measurement(lausanne_expansive_be_t2008, lausanne_minority_protections__expansive_reading, base_extractiveness, 2008, 0.37).
narrative_ontology:measurement_basis(lausanne_expansive_be_t2008, observed).
narrative_ontology:measurement(lausanne_expansive_be_t2023, lausanne_minority_protections__expansive_reading, base_extractiveness, 2023, 0.32).
narrative_ontology:measurement_basis(lausanne_expansive_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_expansive_su_t1923, lausanne_minority_protections__expansive_reading, suppression_requirement, 1923, 0.3).
narrative_ontology:measurement_basis(lausanne_expansive_su_t1923, observed).
narrative_ontology:measurement(lausanne_expansive_su_t1942, lausanne_minority_protections__expansive_reading, suppression_requirement, 1942, 0.38).
narrative_ontology:measurement_basis(lausanne_expansive_su_t1942, observed).
narrative_ontology:measurement(lausanne_expansive_su_t1964, lausanne_minority_protections__expansive_reading, suppression_requirement, 1964, 0.58).
narrative_ontology:measurement_basis(lausanne_expansive_su_t1964, observed).
narrative_ontology:measurement(lausanne_expansive_su_t1971, lausanne_minority_protections__expansive_reading, suppression_requirement, 1971, 0.7).
narrative_ontology:measurement_basis(lausanne_expansive_su_t1971, observed).
narrative_ontology:measurement(lausanne_expansive_su_t2008, lausanne_minority_protections__expansive_reading, suppression_requirement, 2008, 0.56).
narrative_ontology:measurement_basis(lausanne_expansive_su_t2008, observed).
narrative_ontology:measurement(lausanne_expansive_su_t2023, lausanne_minority_protections__expansive_reading, suppression_requirement, 2023, 0.5).
narrative_ontology:measurement_basis(lausanne_expansive_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, identity_coordination).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the Lausanne kernel per the epsilon-invariance principle: the colloquial label 'Lausanne minority protections' covers three structurally distinct claims that cannot share one epsilon. This file (expansive_reading) authors epsilon for the guarantee as broadly read - a protective coordination arrangement whose costs fall on the obligor state. The restrictive_reading authors a different constraint: a minimal individual-worship arrangement with institutional matters routed into general domestic law (different beneficiaries, different victims-or-none, different epsilon). The guarantor_reading adds a supervisory enforcement channel atop the substantive question. The upstream fixed text feeds all three; this reading's scope wins structurally influence the guarantor reading (wider substantive scope gives supervision more to supervise) and foreclose the restrictive reading within any single adjudicative framework. Linked via affects_constraints; each family member carries its own story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__expansive_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
