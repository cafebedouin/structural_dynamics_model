% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Guarantor-Supervision Reading: Internationally Supervised Minority Obligations
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel 'Lausanne
 *   minority protections': the guarantor reading, which holds that the 1923
 *   treaty's minority clauses are internationally supervised obligations
 *   enforceable through guarantor-state diplomacy and European human rights
 *   mechanisms rather than solely through Turkish domestic interpretation.
 *   The standing arrangement under contest — and the referent for every
 *   authored value — is the existing supervision architecture as this reading
 *   assesses it: a treaty-born channel through which Greece raises minority
 *   grievances, minority foundations litigate in Strasbourg, and European
 *   bodies convert the file into benchmarks, while the implementing state
 *   absorbs episodic compliance costs and resists the supervision framing
 *   itself. Per the epsilon-invariance principle, the colloquial label
 *   'Lausanne minority protections' decomposes into three structurally
 *   distinct claims (restrictive, expansive, guarantor readings) with
 *   different epsilon, different beneficiary salience, and different failure
 *   modes; this file authors only the guarantor instantiation and links its
 *   siblings through the network. KEY AGENTS (by structural relationship): -
 *   guarantor_state_greece: agenda-setting beneficiary (institutional/mobile)
 *   — invokes supervision, collects leverage - ecumenical_patriarchate:
 *   principal intended protected party (powerless/identity_locked) -
 *   istanbul_greek_orthodox_communities: protected remnant population
 *   (powerless/constrained) - armenian_community_institutions: protected
 *   foundation estate (organized/constrained) - jewish_community_of_turkey:
 *   protected communal infrastructure (organized/arbitrage) -
 *   western_thrace_muslim_communities: mirror-image protected minority
 *   (organized/constrained) - turkish_state: implementing state bearing
 *   compliance costs (powerful/constrained) — resists supervision framing -
 *   echr_strasbourg_organs: adjudicative administrator
 *   (institutional/analytical) - european_commission_enlargement: leverage
 *   converter (institutional/mobile) - orthodox_diaspora_advocacy_groups:
 *   amplification and funding (organized/arbitrage) -
 *   lausanne_treaty_legal_scholars: analytical observer
 *   (analytical/analytical)
 *
 * KEY AGENTS:
 *   - guarantor_state_greece: agenda-setting beneficiary (institutional/mobile) — co-authored the treaty, periodically invokes the minority clauses across bilateral, parliamentary, and EU venues, and collects diplomatic leverage from each invocation while spending ministerial capital
 *   - ecumenical_patriarchate: principal intended protected party (powerless/identity_locked) — heads an Istanbul-based see whose schools, foundations, and clergy pipeline depend on Turkish implementing law; relocation would dissolve its canonical identity, so it pursues survival through Strasbourg litigation, diplomatic appeals, and quiet negotiation
 *   - istanbul_greek_orthodox_communities: protected remnant population (powerless/constrained) — a few thousand families plus Imbros/Tenedos villagers holding minority schooling and foundation-property rights after catastrophic mid-century shrinkage
 *   - armenian_community_institutions: protected foundation estate (organized/constrained) — runs dozens of schools and charities, filed the bulk of post-2011 property-registration claims, and lost most historic holdings to earlier confiscations
 *   - jewish_community_of_turkey: protected communal infrastructure (organized/arbitrage) — maintains a small institutional footprint with a well-worn emigration exit exercised repeatedly after the 1942 levy and 1955 riots
 *   - western_thrace_muslim_communities: mirror-image protected minority (organized/constrained, spoken-for) — holds reciprocal minority status in Greece but its priorities travel mainly through Athens and Ankara rather than through its own seat
 *   - turkish_state: implementing state bearing compliance costs (powerful/constrained) — implements through its own courts and foundations directorate, treats the clauses as settled domestic law, resists outside supervision, and absorbs episodic judgment debts, returned properties, and accession-benchmark pressure while controlling the levers outsiders most request
 *   - echr_strasbourg_organs: adjudicative administrator (institutional/analytical) — receives individual applications, adjudicates property and education treatment against Convention standards, and issues judgments whose follow-through rests on Committee of Ministers supervision
 *   - european_commission_enlargement: leverage converter (institutional/mobile) — turns the minority file into accession benchmarks and progress-report items; the file rises and falls with enlargement politics
 *   - orthodox_diaspora_advocacy_groups: amplification and funding (organized/arbitrage) — US- and Europe-based heritage organizations that fund litigation and publish monitoring with no exposure to implementing decisions
 *   - lausanne_treaty_legal_scholars: analytical observer (analytical/analytical) — documents drafting history and tracks which clauses produced justiciable practice versus dormant text
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.31).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.38).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Guarantor-Supervision Reading: Internationally Supervised Minority Obligations").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/religious_governance/minority_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, 'bc62ae89-ee4b-4e66-994e-37c492cec0d6').
narrative_ontology:cs_kernel_codification('bc62ae89-ee4b-4e66-994e-37c492cec0d6', fixed_text).
narrative_ontology:cs_authority_grounding('bc62ae89-ee4b-4e66-994e-37c492cec0d6', lineage).
narrative_ontology:cs_interpretation_layer_present('bc62ae89-ee4b-4e66-994e-37c492cec0d6').
narrative_ontology:cs_reading_relation('bc62ae89-ee4b-4e66-994e-37c492cec0d6', lausanne_minority_protections__restrictive_reading, influences).
narrative_ontology:cs_reading_relation('bc62ae89-ee4b-4e66-994e-37c492cec0d6', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('bc62ae89-ee4b-4e66-994e-37c492cec0d6', foundational, lausanne_obligations_internationally_supervised).
narrative_ontology:cs_axiom_status(lausanne_obligations_internationally_supervised, holdable).
narrative_ontology:cs_axiom_grounding('bc62ae89-ee4b-4e66-994e-37c492cec0d6', lausanne_obligations_internationally_supervised, conventional).
narrative_ontology:cs_axiom('bc62ae89-ee4b-4e66-994e-37c492cec0d6', secondary, guarantor_and_strasbourg_channels_are_valid_venues).
narrative_ontology:cs_axiom_status(guarantor_and_strasbourg_channels_are_valid_venues, holdable).
narrative_ontology:cs_axiom_grounding('bc62ae89-ee4b-4e66-994e-37c492cec0d6', guarantor_and_strasbourg_channels_are_valid_venues, conventional).
narrative_ontology:cs_reference_frame('bc62ae89-ee4b-4e66-994e-37c492cec0d6', league_era_full_supervision_regime).
narrative_ontology:cs_drift_state('bc62ae89-ee4b-4e66-994e-37c492cec0d6', post_1990_strasbourg_petition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bc62ae89-ee4b-4e66-994e-37c492cec0d6', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, guarantor_state_greece).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, ecumenical_patriarchate).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, istanbul_greek_orthodox_communities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, armenian_community_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, jewish_community_of_turkey).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, western_thrace_muslim_communities).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, orthodox_diaspora_advocacy_groups).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, international_minority_supervision_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, individual_petition_effectiveness_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Co-signed the 1923 treaty and periodically invokes its minority clauses in bilateral dealings, parliamentary resolutions, and EU contexts, pressing for seminary reopening, foundation-property restitution, and recognition of the Istanbul church leadership's standing. Collects diplomatic leverage whenever the file is raised and spends ministerial time and political capital on it; can escalate to EU and Council of Europe venues or quietly set the file aside when wider relations matter more.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, guarantor_state_greece, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, guarantor_state_greece, beneficiary).

% Heads an Istanbul-based see whose schools, foundations, and clergy-training pipeline depend on Turkish implementing law; moving would dissolve its canonical identity, so survival runs through Strasbourg litigation, appeals to visiting officials, and quiet negotiation with Ankara. Received court-ordered returns of foundation properties in recent decades while its theological academy has remained closed since 1971.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, ecumenical_patriarchate, beneficiary,
    powerless, civilizational, identity_locked, global).

% Remnant lay population of a few thousand, plus Imbros and Tenedos villages, holding minority schooling and foundation-property rights; shrank catastrophically after the 1942 capital levy, the 1955 riots, and the 1964 deportations. Families exit by emigration while remaining community boards fight case-by-case registration battles for orphanages and school buildings.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, istanbul_greek_orthodox_communities, beneficiary,
    powerless, generational, constrained, local).

% Operates dozens of schools, churches, and charitable foundations under the state foundations directorate; filed the largest share of property-registration claims after the 2011 registration decree and pursued parallel cases in Strasbourg. Most historic holdings were lost to earlier confiscations and auctions, and each recovery arrives case by case.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, armenian_community_institutions, beneficiary,
    organized, generational, constrained, national).

% Maintains a small communal infrastructure of schools, a hospital, and synagogues under treaty-listed minority status. Large waves of departure followed the 1942 wealth levy and the 1955 riots, so remaining institutions weigh every dispute against a well-worn emigration path already exercised by most of the former community.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, jewish_community_of_turkey, beneficiary,
    organized, biographical, arbitrage, national).

% Holds mirror-image minority status in Greece — muftiates, charitable endowments, bilingual schools — and stands to gain whenever scrutiny of the minority clauses applies to both signatories. Its own priorities, such as elected muftis and school curricula, travel mainly through Athens and Ankara rather than through any seat of its own in the diplomatic process.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, western_thrace_muslim_communities, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, western_thrace_muslim_communities, excluded).

% Implements minority policy through its own courts, the general directorate of foundations, and interior-ministry practice; treats the treaty clauses as settled domestic law to be interpreted by Turkish institutions and resists framings of outside supervision. Absorbs episodic costs — judgment debts, returned properties, diplomatic friction, accession benchmarks — while controlling the specific levers external parties most request, such as reopening the seminary and recognizing the Istanbul see's broader standing. Leaving the treaty framework or the Council of Europe is possible in principle but prohibitively entangled with its wider economic and security ties.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, payer,
    powerful, generational, constrained, national).

% Receives individual applications from minority foundations and community members, adjudicates whether property and education treatment meets Convention standards, and issues judgments carrying compensation awards. Caseload grows with each filing cycle, and follow-through depends on Committee of Ministers supervision rather than any power of its own to compel implementation.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, echr_strasbourg_organs, agenda_setter,
    institutional, generational, analytical, continental).

% Converts minority-clause grievances into accession progress-report items and negotiating benchmarks, giving the diplomatic file a second leverage channel. The whole apparatus rises and falls with enlargement politics: when accession talks stalled, the file lost its primary vehicle regardless of its substantive merits.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, european_commission_enlargement, agenda_setter,
    institutional, biographical, mobile, continental).

% US- and Europe-based heritage organizations that fund litigation, lobby legislatures, and publish monitoring reports. They gain standing and donor engagement from the issue and face no exposure whatsoever to the implementing state's decisions; their participation is entirely voluntary and reversible.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, orthodox_diaspora_advocacy_groups, beneficiary,
    organized, biographical, arbitrage, global).

% Documents the treaty's drafting history, tracks which clauses produced justiciable practice versus dormant text, and publishes comparative studies of League-era minority protection. Holds no stake in outcomes and supplies the archive on which later readers of the arrangement rely.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, lausanne_treaty_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, diffuse).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common, externally verifiable channel for raising and assessing cross-border minority-protection grievances that neither community accepts the implementing state's domestic institutions alone to judge impartially; standardizes what counts as a treaty shortcoming across guarantor diplomacy and Strasbourg litigation, and gives both signatories' obligations a shared reference record.
% TRANSFER_FUNCTION: Moves adjudicative attention and diplomatic leverage toward minority grievances and toward the guarantor state's broader bilateral agenda; moves compliance costs — restitution, compensation awards, reputational exposure, accession benchmarks — onto the implementing state; moves legitimacy and jurisdictional resources to the European institutions that operate the channel.
% ABSENT_VOICES: The Thrace communities are spoken for rather than speaking (voiced through Athens and Ankara, with their own priority disputes underrepresented); ordinary members of the Istanbul Armenian and Jewish communities beyond foundation boards have no procedural seat; the Turkish domestic public has never been consulted on the terms of external supervision it is asked to accept; and the generations educated before the seminary's 1971 closure — the clergy cohort the pipeline was meant to produce — exist only as a missing class in the record. All of them sit outside the guarantor capitals and the Strasbourg chamber where the file circulates.
% DISAPPEARANCE_RATIONALE: If the supervision reading vanished overnight, the implementing state would experience immediate relief from judgment exposure and benchmark pressure and would claim nothing else changes, since it already treats interpretation as domestic; the protected communities would lose the one channel that has ever returned property or compensated losses at scale, shifting them back to wholly domestic remedies they do not trust; the guarantor state would lose a recurring leverage asset and reframe the file around reciprocity and EU criteria; the Strasbourg organs would continue hearing Convention cases stripped of their treaty-clause framing. Whether the world rearranges therefore depends on which seat answers — hence contested rather than a single verdict.
% FOUNDING_PROBLEM: After the Ottoman collapse and the Balkan Wars, the emerging nation-states inherited religiously governed populations amid a recent record of wartime persecution; the powers demanded treaty-guaranteed minority protection as the price of recognition, supervised from outside because domestic guarantees were judged untrustworthy — the arrangement was built to bridge a transition until the new states matured into reliable protectors.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists from outside the benefiting parties: the League of Nations archival record and its successor UN treaty-body files document the founding design of external supervision and its provisional intent; ECtHR judgment recitals independently cite the treaty's protective purpose when adjudicating property and schooling cases; and the implementing state's own sustained rejection of the supervision framing — arguing the clauses are settled domestic law — implicitly attests that the founding premise was external supervision, since that is precisely what it refuses. British Foreign Office correspondence from the 1922-23 negotiation window supplies a fourth, adversarial-neutral source. No corroboration comes only from within the beneficiary set.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, contested).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.31, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.31 at interval end) because, assessed by this reading's own lights, the arrangement's costs on the implementing state are legitimate oversight burdens, and in fact the channel is too weak to move much value: enforcement succeeds episodically (court-ordered property returns, compensation awards) and fails routinely (seminary closed since 1971 despite decades of invocation). Suppression (0.38) is a raw structural property, unscaled by power or scope: the arrangement adds an adjudicative channel rather than foreclosing alternatives, but sustaining it requires continuous diplomatic and litigious effort against an unwilling implementer. Theater (0.35) reflects a genuine split: annual demarches and recycled progress-report paragraphs that change nothing, alongside real judgments with material consequences. Accessibility collapse is low (0.35) — minorities retain domestic courts, UN treaty bodies, bilateral channels, and emigration; the arrangement supplements rather than monopolizes. Resistance is high (0.62) because the implementing state actively contests the supervision framing itself: denying the see's standing, slow-walking restitution, and asserting domestic-exclusivity of interpretation. The measurement series run on ONE shared grid (t = 0, 15, 30, 45, 60, 75, 90, 100; years since 1923) across all three tracked metrics. The trajectory is a full rise-collapse-rebuild cycle: League-era enforcement machinery (high suppression_requirement at t0) decays to near-zero by t45 as the host institution dies; the European petition pathway rebuilt enforcement capacity after t60-t75; theater peaks in the dormancy valley (0.60 at t45) where ritual invocation replaced function. The cycle is driven by enforcement-infrastructure availability (host institutions rising and falling), not by intermittent reinforcement — the oscillation is exogenous, not an extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharply different per-seat types from this structural data. From the implementing state's seat (sole victim declaration, powerful but constrained exit), the arrangement presents as externally imposed supervision with real episodic costs and a sovereignty-framing dispute — a high-directionality experience of the same structure the guarantor experiences as service. From the guarantor seat (agenda-setting beneficiary, mobile exit, continental scope), the arrangement is a leverage asset it activates selectively. From the trapped and identity-locked minority seats, it is a lifeline venue whose weakness is itself the injury. Same-nominal-level institutional actors diverge on role and exit: the two states differ in exit mobility and structural position; the Strasbourg organs and the enlargement machinery monetize the file in jurisdiction and benchmarks respectively. Identity-lock dynamics concentrate in the patriarchate: its professional-canonical identity is constituted by location and succession in Istanbul, so its exit option is not merely costly but self-dissolving; if that identity frame broke (a movable see became conceivable), its directionality would shift toward mobile and the computed seat profile would flatten.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The seven declared beneficiaries (protected communities and institutions, plus the guarantor state) derive low directionality — the arrangement subsidizes them with venue, restitution, and leverage. The single declared victim, turkish_state, derives high directionality (near-full target) from victim status plus constrained exit: it bears judgment debts, returned-property costs, and benchmark pressure and cannot cheaply abandon the treaty or Council membership. One override is declared: the institutional power atom is pinned to d = 0.12 because the derivation chain has no beneficiary declarations for the treaty-administering bodies (Strasbourg organs, enlargement machinery) and its canonical institutional fallback would place them mid-scale; structurally these seats are subsidized by the arrangement (each invocation expands their jurisdiction, caseload, and agenda relevance), so the explicit override records their near-beneficiary position alongside the guarantor state, which shares the atom and genuinely nets leverage. The override is safe from collision: the implementing state sits on the powerful atom, not the institutional one. Note again that suppression enters the computation unscaled; only extractiveness is scaled by directionality and spatial scope, so the implementing state's continental-scale verification difficulty moderately amplifies its experienced chi while doing nothing to the raw suppression figure.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading exists to prevent two opposite mislabels. Read without the guarantor frame, the arrangement looks like dead-letter text: decades of dormancy, ritual demarches, a closed seminary — a candidate for inertial persistence with high theatrical maintenance. Read with the frame but without the temporal record, the post-1990 litigation revival looks like a healthy coordination mechanism. The truth is a scaffold whose sunset was never codified: the arrangement was designed as transitional support (League-era minority protection was explicitly provisional, awaiting the implementer's maturation into trustworthy protector), the transition it supported was violently overtaken by events (exchange, levy, riots, expulsions) rather than completed, and the mechanism neither dissolved nor matured — it calcified into semi-ritual practice while retaining a live adjudicative edge. The R5 mismatch consumer reads founding_problem_status (contested) against disappearance_verdict (contested): no hard zombie flag fires, but the theater series' dip-and-return shape marks the drift watch-point — if theater_ratio climbs past 0.5 again while case throughput flattens, the inertial reading wins and reclassification review is warranted. The claim/metric independence rule is honored here deliberately: the scaffold claim encodes design intent and transitional function; the metrics encode observed weak, resistible, partly theatrical operation. Where the engine's computed per-seat types diverge from the claim — particularly the implementing state's seat computing as a substantially constrained party — that divergence is the measurement this story exists to take, not an inconsistency to reconcile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_epsilon_routing,
    'This constraint is one reading (guarantor_reading) of the kernel lausanne_minority_protections; how much of the measured profile is reading-indexed rather than topic-fixed?',
    'Corpus-level comparison against the sibling readings: restrictive_reading authors epsilon over a domestic-exclusivity arrangement (expects low epsilon from its own lights), expansive_reading authors epsilon over unfulfilled institutional-continuity guarantees (expects high epsilon). The disagreement is located on the locus-of-authority axis (international supervision versus domestic interpretation) intersecting the scope axes (worship-only versus institutional continuity) carried by the siblings.',
    'If the sibling stories author materially different epsilon over the same historical record, the kernel decomposes cleanly into three epsilon-invariant constraints; if they converge, the reading distinction is rhetorical and the family collapses into one story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_epsilon_routing, conceptual, 'Reading-indexed nature of epsilon for one instantiation of the Lausanne kernel.').

omega_variable(
    strasbourg_pathway_durability,
    'Is the post-1990 European adjudication channel a durable second enforcement pillar of the supervision arrangement, or an epiphenomenon contingent on Council of Europe membership politics?',
    'Track whether minority-clause litigation volume and compliance outcomes survive episodes of Turkish disengagement rhetoric, budget crises, and suspension debates; compare case throughput before and after each membership shock.',
    'If epiphenomenal, the arrangement degrades toward ritual diplomacy with negligible functional share (theater_ratio rising past 0.5); if durable, the transitional-support function retains a working edge and the scaffold reading stays defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strasbourg_pathway_durability, empirical, 'Durability of the European adjudication pillar of the guarantor channel.').

omega_variable(
    reciprocity_operational_symmetry,
    'Does the supervision arrangement in practice bind both signatory states symmetrically, or does it operate one-directionally in favor of the guarantor state''s agenda?',
    'Compare invocation frequency and litigation outcomes for minority clauses as applied to each state: Thrace-community cases (mufti appointment, waqf administration) against Istanbul-community cases (foundation property, seminary reopening).',
    'If one-directional, the arrangement functions as a unilateral diplomatic instrument wearing a mutual-obligation form, which raises the effective extraction asymmetry between the two implementing states and pressures the classification away from pure transitional support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_operational_symmetry, empirical, 'Symmetry of supervision across the two implementing states.').

omega_variable(
    missing_sunset_design_intent,
    'The arrangement was conceived as transitional support pending full civic integration of the protected populations, yet carries no codified sunset — is the omission a League-era drafting artifact or a load-bearing ambiguity deliberately preserved to keep the leverage alive?',
    'Diplomatic archival analysis of the 1922-23 negotiations and subsequent guarantor-state internal memoranda: did drafters discuss termination conditions, and did later foreign ministries treat permanence as a feature?',
    'Deliberate preservation would recast the arrangement as a permanent diplomatic instrument rather than transitional support, challenging the authored scaffold claim in favor of an open-ended coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(missing_sunset_design_intent, preference, 'Whether the absent sunset clause reflects design intent or drafting accident.').

omega_variable(
    dormancy_period_measurement_gap,
    'During the long dormancy (roughly interval points 30-60) the measured extraction and suppression fall to their lowest values precisely while the protected populations suffered their largest catastrophic losses (1942 capital levy, 1955 riots, 1964 expulsions) with no supervisory response — does the low measured profile describe a benign arrangement or an absent one?',
    'Counterfactual weighting of the dormancy window: score the arrangement by harm-per-invocation rather than gross activity, using archival records of failed or unmade interventions against the loss events.',
    'If absence, the historical trough understates the arrangement''s failure and any lifecycle analysis should weight the dormancy gap as the decisive segment; the revival-era recovery would read as reconstruction rather than improvement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_period_measurement_gap, empirical, 'Whether the dormancy-era low metrics measure benignity or nonexistence of the arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lgp_guarantor_tr_t0, lausanne_minority_protections__guarantor_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t0, observed).
narrative_ontology:measurement(lgp_guarantor_tr_t15, lausanne_minority_protections__guarantor_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t15, observed).
narrative_ontology:measurement(lgp_guarantor_tr_t30, lausanne_minority_protections__guarantor_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t30, observed).
narrative_ontology:measurement(lgp_guarantor_tr_t45, lausanne_minority_protections__guarantor_reading, theater_ratio, 45, 0.6).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t45, observed).
narrative_ontology:measurement(lgp_guarantor_tr_t60, lausanne_minority_protections__guarantor_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t60, observed).
narrative_ontology:measurement(lgp_guarantor_tr_t75, lausanne_minority_protections__guarantor_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t75, observed).
narrative_ontology:measurement(lgp_guarantor_tr_t90, lausanne_minority_protections__guarantor_reading, theater_ratio, 90, 0.25).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t90, observed).
narrative_ontology:measurement(lgp_guarantor_tr_t100, lausanne_minority_protections__guarantor_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement_basis(lgp_guarantor_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(lgp_guarantor_be_t0, lausanne_minority_protections__guarantor_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(lgp_guarantor_be_t0, observed).
narrative_ontology:measurement(lgp_guarantor_be_t15, lausanne_minority_protections__guarantor_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement_basis(lgp_guarantor_be_t15, observed).
narrative_ontology:measurement(lgp_guarantor_be_t30, lausanne_minority_protections__guarantor_reading, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(lgp_guarantor_be_t30, observed).
narrative_ontology:measurement(lgp_guarantor_be_t45, lausanne_minority_protections__guarantor_reading, base_extractiveness, 45, 0.18).
narrative_ontology:measurement_basis(lgp_guarantor_be_t45, observed).
narrative_ontology:measurement(lgp_guarantor_be_t60, lausanne_minority_protections__guarantor_reading, base_extractiveness, 60, 0.2).
narrative_ontology:measurement_basis(lgp_guarantor_be_t60, observed).
narrative_ontology:measurement(lgp_guarantor_be_t75, lausanne_minority_protections__guarantor_reading, base_extractiveness, 75, 0.3).
narrative_ontology:measurement_basis(lgp_guarantor_be_t75, observed).
narrative_ontology:measurement(lgp_guarantor_be_t90, lausanne_minority_protections__guarantor_reading, base_extractiveness, 90, 0.34).
narrative_ontology:measurement_basis(lgp_guarantor_be_t90, observed).
narrative_ontology:measurement(lgp_guarantor_be_t100, lausanne_minority_protections__guarantor_reading, base_extractiveness, 100, 0.31).
narrative_ontology:measurement_basis(lgp_guarantor_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(lgp_guarantor_su_t0, lausanne_minority_protections__guarantor_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(lgp_guarantor_su_t0, observed).
narrative_ontology:measurement(lgp_guarantor_su_t15, lausanne_minority_protections__guarantor_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(lgp_guarantor_su_t15, observed).
narrative_ontology:measurement(lgp_guarantor_su_t30, lausanne_minority_protections__guarantor_reading, suppression_requirement, 30, 0.25).
narrative_ontology:measurement_basis(lgp_guarantor_su_t30, observed).
narrative_ontology:measurement(lgp_guarantor_su_t45, lausanne_minority_protections__guarantor_reading, suppression_requirement, 45, 0.18).
narrative_ontology:measurement_basis(lgp_guarantor_su_t45, observed).
narrative_ontology:measurement(lgp_guarantor_su_t60, lausanne_minority_protections__guarantor_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement_basis(lgp_guarantor_su_t60, observed).
narrative_ontology:measurement(lgp_guarantor_su_t75, lausanne_minority_protections__guarantor_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(lgp_guarantor_su_t75, observed).
narrative_ontology:measurement(lgp_guarantor_su_t90, lausanne_minority_protections__guarantor_reading, suppression_requirement, 90, 0.46).
narrative_ontology:measurement_basis(lgp_guarantor_su_t90, observed).
narrative_ontology:measurement(lgp_guarantor_su_t100, lausanne_minority_protections__guarantor_reading, suppression_requirement, 100, 0.38).
narrative_ontology:measurement_basis(lgp_guarantor_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Lausanne minority protections' conflates three structurally distinct claims with materially different epsilon values, decomposed per the epsilon-invariance principle into a three-story constraint family: restrictive_reading (domestic exclusivity, worship-only scope; its own lights expect low epsilon), expansive_reading (institutional-continuity scope; unfulfilled guarantees assessed as violated trust expect high epsilon), and this file's guarantor_reading (external supervision locus; low epsilon as transitional support with a weak edge). The guarantor reading sits upstream of both siblings in influence terms: every successful externally-adjudicated case erodes the restrictive premise's operating environment, and the adjudication channel is the delivery vehicle the expansive claims require to bite. Neither dependency is a logical foreclosure — locus and scope are independent axes — so all three readings remain simultaneously live positions held by different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lausanne_minority_protections__guarantor_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
