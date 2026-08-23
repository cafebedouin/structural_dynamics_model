% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Liturgical-Preservation Criterion for Hebrew Linguistic Life
 *   domain: Sociolinguistics / Religious Studies / Nationalism Studies
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested kernel
 *   hebrew_linguistic_life: the liturgical-preservation reading, under which
 *   Hebrew was never a dead language, because linguistic life subsists in the
 *   unbroken recitation, study, and transmission of its sacred texts —
 *   regardless of whether anyone speaks the language at home. On this reading
 *   the custody arrangement (synagogue cycle, study academies, scribal norms,
 *   rabbinic adjudication) is the language's life, sustained from the
 *   Mishnaic consolidation through the present; the nineteenth-century
 *   vernacular revival added nothing that was missing and is assessed as
 *   profanation rather than resurrection. KEY AGENTS (by structural
 *   relationship): - rabbinic_leadership: Agenda-setting custodian
 *   (institutional / identity_locked) — fixes rite, curriculum, and bans;
 *   collects deference and adjudication authority - yeshiva_system:
 *   Collecting intermediary (organized / constrained) — channels students'
 *   labor and endowments; administers within fixed boundaries -
 *   diaspora_kehillot: Net coordinating beneficiary (organized / mobile) —
 *   buys canon portability and cross-border unity with assessments -
 *   heder_children: Primary cost-bearing seat (powerless / trapped) —
 *   childhood allocated to an unspoken register - heterodox_intellectuals:
 *   Disciplined cost-bearers (moderate / constrained) — banned,
 *   excommunicated, or severed - women_barred_from_mastery_track:
 *   Structurally silenced cost-bearers (powerless / identity_locked) —
 *   excluded from the custody role the criterion defines -
 *   sacred_hebrew_register: Non-actor entity (recorded with agent=false) —
 *   the reading's designated injured party - israeli_secular_speakers:
 *   Excluded voice (powerful / arbitrage) — fluent speakers the criterion
 *   declines to count - academic_linguists: Analytical observer (analytical /
 *   analytical) — documents and compares, decides nothing Decomposition
 *   discipline: the label 'was Hebrew alive?' covers three structurally
 *   distinct claims (see network.dual_formulation_note); this file authors
 *   only the liturgical reading, with its own epsilon and parties.
 *   Claim/metric independence: claimed_type 'mountain' states this reading's
 *   own commitment — that the criterion is grounded in the intrinsic sanctity
 *   of the holy tongue rather than in any human arrangement — while the
 *   authored metrics describe the custody arrangement's actual operation,
 *   enforcement dependence included. The divergence between the reading's
 *   claim and the computed type is precisely the false-summit measurement
 *   this corpus takes; it is not reconciled here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.35).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.55).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, mountain).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Liturgical-Preservation Criterion for Hebrew Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "Sociolinguistics / Religious Studies / Nationalism Studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).
domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '808e154b-5b4a-41ed-a633-80c38fce55c0').
narrative_ontology:cs_kernel_codification('808e154b-5b4a-41ed-a633-80c38fce55c0', fixed_text).
narrative_ontology:cs_authority_grounding('808e154b-5b4a-41ed-a633-80c38fce55c0', lineage).
narrative_ontology:cs_interpretation_layer_present('808e154b-5b4a-41ed-a633-80c38fce55c0').
narrative_ontology:cs_reading_relation('808e154b-5b4a-41ed-a633-80c38fce55c0', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_reading_relation('808e154b-5b4a-41ed-a633-80c38fce55c0', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('808e154b-5b4a-41ed-a633-80c38fce55c0', foundational, consecrated_transmission_constitutes_life).
narrative_ontology:cs_axiom_status(consecrated_transmission_constitutes_life, holdable).
narrative_ontology:cs_axiom_grounding('808e154b-5b4a-41ed-a633-80c38fce55c0', consecrated_transmission_constitutes_life, theological).
narrative_ontology:cs_axiom('808e154b-5b4a-41ed-a633-80c38fce55c0', secondary, secular_vernacular_appropriation_desecrates).
narrative_ontology:cs_axiom_status(secular_vernacular_appropriation_desecrates, holdable).
narrative_ontology:cs_axiom_grounding('808e154b-5b4a-41ed-a633-80c38fce55c0', secular_vernacular_appropriation_desecrates, theological).
narrative_ontology:cs_reference_frame('808e154b-5b4a-41ed-a633-80c38fce55c0', unbroken_masoratic_chain).
narrative_ontology:cs_drift_state('808e154b-5b4a-41ed-a633-80c38fce55c0', post_emancipation_secular_revival_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('808e154b-5b4a-41ed-a633-80c38fce55c0', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_leadership).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_system).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_kehillot).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_hebrew_register).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, heder_children).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, heterodox_intellectuals).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, women_barred_from_mastery_track).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, masorah_unbroken_chain_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, lashon_hakodesh_sanctity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordains successors, fixes the liturgical rite and study curriculum, and convenes the courts that license Hebrew books and pronounce bans. Deference, adjudication authority, and later-era certifier income flow through their custody of the transmission chain. Leaving the role forfeits the standing the role consists of.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Advanced academies receive communal funds, endowed support, and their students' prime working years; their heads set curricula and discipline within boundaries the leadership fixes. Continuity depends wholly on the enrollment and endowment flows the custody arrangement channels to them.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_system, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, yeshiva_system, agenda_setter).

% Scattered communities get a portable canon, a shared calendar and legal idiom, and a common literate channel linking merchants, marriage brokers, and courts across political borders; they pay assessments sustaining scholars and schools. Exit exists — migration, assimilation, vernacular-only life — at the price of communal standing.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_kehillot, beneficiary,
    organized, generational, mobile, global).

% Children from roughly age three spend the school day memorizing scripture and recitation formulas in a register nobody speaks at home, under family and communal expectation, with no schooling alternative inside the community. The realistic choice is compliance now or family rupture later.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, heder_children, payer,
    powerless, immediate, trapped, local).

% Maskilim, reformers, and later secular Hebraists who proposed treating the texts as literature or moving worship into vernacular met book bans, blocked publication licenses, excommunication decrees, and severed family ties; some emigrated or converted, others publicly recanted.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, heterodox_intellectuals, payer,
    moderate, biographical, constrained, continental).

% Exempted from the commanded study duty that defines the custodial role, most women across the tradition's history were taught neither to read the canon in the original nor to enter the transmission professions; piety ran through domestic observance and vernacular devotional works instead. Exit runs through the same door as everyone's: leaving the community altogether.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, women_barred_from_mastery_track, payer,
    powerless, biographical, identity_locked, global).

% Recorded as a non-actor entity for completeness: the corpus itself together with its consecrated usage-norms. On this reading the register is what stands to be injured — its prayer-formulas carried into newspapers, parliamentary halls, and street speech without consecrating intent. It holds no preferences and collects nothing; its entry marks where the reading locates injury to the tradition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_hebrew_register, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_hebrew_register).

% Fluent everyday speakers of revived Hebrew whose linguistic condition the criterion declines to count. They stand wholly outside this framework's adjudicating councils — rabbinic courts, academy heads — and run their own demographic and functional measures instead; their voice on when Hebrew is alive enters this framework only as an object of rulings, never as a participant in making them.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, israeli_secular_speakers, excluded,
    powerful, generational, arbitrage, national).

% Comparative sociolinguists and Semiticists who document the transmission record, model liturgical-register survival alongside the Latin, Ge'ez, and Coptic cases, and classify Hebrew's trajectory by demographic and functional measures. They publish and testify; they neither administer the practice nor bear its costs.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, academic_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_leadership).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains one canonical sacred corpus and the trained competence to reproduce it across dispersed, vernacularly diverse communities with no state behind it: standardized recitation, the annual reading cycle, scribal copying norms, and a portable scholarly idiom that lets a court in Cairo correspond with one in Mainz. Solves canon stability, elite intercommunal communication, and identity continuity under dispersion in a single structure.
% TRANSFER_FUNCTION: Moves children's formative years (study labor), communal assessments, and deference upward to the rabbinic and academy tiers; moves custody of the sacred register downward through ordained chains; moves dissenters outward, through ban and exclusion.
% ABSENT_VOICES: Israeli secular speakers and the maskilim's intellectual heirs would object that aliveness is measured demographically or functionally, not liturgically — they are absent from this framework's adjudicating councils entirely. Women barred from the mastery track are inside the community yet historically voiceless in its councils about who may count as a custodian. All three objections are recorded only here.
% DISAPPEARANCE_RATIONALE: Overnight removal snaps the transmission chains within a generation or two: recitation lapses into translation or silence, scribal competence dies with its last holders, rabbinic adjudication loses its textual warrant, and the kehillot lose the shared canon that made them one people across exile. Nothing in logistics or nature changes; everything organized around the custody does.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's mother tongue, the covenantal text and practice had to be kept legible, authoritative, and reproducible across scattered vernacular-speaking communities with no territorial institutions — custody of the canon through exile.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties on the problem itself: the manuscript record (Qumran scrolls through Masoretic codices) attests a real transmission problem and a real solution; comparative sociolinguistics confirms continuous liturgical use does sustain a register's institutional life, citing the parallel careers of Latin, Ge'ez, and Coptic; historians of Judaism across secular academies attest the coordination achievement while disputing this reading's exclusivity claim. No source outside the tradition attests the descent-based sanctity premise — corroboration covers the founding problem, not the theology.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, ExtMetricName, E),
    domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hebrew_linguistic_life__liturgical_preservation_reading),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (endpoint 0.35) because, assessed by this reading's own lights, the custody arrangement is devotion first: costs are framed as commanded obligations, participation is covenantal, and no commercial rent structure sits at the center. The residual 0.35 records what even the reading concedes — compulsory childhood curricula, sanctioned dissent, and gendered exclusion from the mastery tier. Suppression (0.55, raw and unscaled — only extractiveness is scaled by directionality and scope) reflects real enforcement machinery: excommunication, publication licensing, communal discipline; it is high enough that the arrangement could not persist on consent alone, low enough that exit (migration, assimilation, conversion) remained possible throughout. Theater stays low (0.20) because the practice is the function: recitation and study are the very activity the criterion calls linguistic life, with only a modest ceremonial drift among less-literate participants. Accessibility_collapse (0.38) and resistance (0.62) are honestly non-mountain-shaped: the rival criteria remain fully statable — two sibling files instantiate them — and the criterion met sustained resistance from maskilim, reformers, secular Hebraists, and modern demography-minded linguists. The suppression_requirement series is authored deliberately because enforcement-capacity change IS this story's traced dynamic: a five-century build-up (gaonate discipline, print-censorship peak around 1500, anti-Haskalah bans peaking near 1800), collapse after emancipation stripped kehillah coercive teeth, and partial re-hardening inside the voluntary core by 2020. All three series share one eight-point grid (200, 600, 1000, 1500, 1800, 1880, 1948, 2020) so no metric row borrows an end-state value from another. Coalition note: the dispersed cost-bearers (children, women, dissenters) never achieved coalition capacity — separated by age, geography, and gender-role segregation — which is why the enforcement machinery held as long as it did.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the structural data is built to make them: from the identity_locked custodian seat, the arrangement is a covenantal good one is fused to; from the kehillot seat it is a purchased coordination benefit; from the trapped-child and silenced-women seats it is compulsory allocation of life-years to an unspoken register; from the excluded secular-speaker seat it is simply not operative — a criterion applied by councils they will never sit on. Same nominal tradition, four different experienced structures. The engine computes per-seat classifications from power, exit options, and directional position; nothing here adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation, so no overrides are needed. rabbinic_leadership (declared beneficiary, agenda_setter, identity_locked) sits near the beneficiary pole: the chain's custody subsidizes its authority. yeshiva_system and diaspora_kehillot derive low-to-moderate d as beneficiaries, the kehillot slightly higher because their benefit is diffuse and their exit (mobility, assimilation) is real. heder_children, heterodox_intellectuals, and women_barred_from_mastery_track derive high d as declared victims with trapped, constrained, and identity_locked exits respectively — the women's identity_lock pushing them toward the full-target end despite lacking any formal sanction against them. sacred_hebrew_register is declared in victims to honor the reading's own injury-claim, but carries agent=false, so it is excluded from derivation and feeds no directionality arithmetic — an entity must not collect chi as if it were a party. israeli_secular_speakers and academic_linguists sit outside the beneficiary/victim sets and derive accordingly (excluded and analytical respectively).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview comes back coherent: the founding problem (custody of a canonical text through dispersion) is live by the tradition's own testimony and by outside corroboration of the transmission record, and disappearance would rearrange the world — so the status x verdict pair raises no zombie or capture flag, and mandatrophy is not declared. Classification discipline cuts both ways: against the pure-snare mislabel, because the coordination achievement is real and enormous (an unbroken canon across eighteen centuries of stateless dispersion is a collective-action success few institutions match); against the pure-rope acceptance, because the asymmetric burdens — childhood labor, banned books, half the community barred from the defining competency — are equally real and enforcement-dependent. The tangled middle is where the structural data puts this arrangement, whatever the reading's own mountain-claim asserts; that gap is the datum.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_custodial_construction,
    'Is the criterion — that linguistic life subsists in consecrated transmission regardless of vernacular use — a genuine natural-theological law grounded in the register''s intrinsic sanctity, or a constructed institutional constraint that concentrates custody rents in identifiable custodial elites?',
    'Comparative reception history across independent liturgical-language regimes (Latin Christendom, Ge''ez Ethiopia, Coptic Egypt, scriptural-Arabic pedagogy): if every regime adopting this criterion develops the same custodial rent structure and dissent-suppression machinery regardless of its theology, construction is indicated; convergence confined to communities sharing this specific theology supports the reading''s own grounding.',
    'Resolves the false-summit question for this story: certification as a discovered natural limit versus recognition of a coordination-plus-extraction hybrid, and with it which remedial questions are even coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_custodial_construction, conceptual, 'Whether the aliveness criterion is discovered law or built institution benefiting custodians.').

omega_variable(
    kernel_reading_boundary_dispute,
    'This constraint instantiates liturgical_preservation_reading of kernel hebrew_linguistic_life, while native_generational_reading draws the aliveness boundary at native child acquisition and marketplace_pidgin_reading draws it at practical inter-communal function — which structural element do the three readings actually contest?',
    'Parameter isolation: all three accept the same historical record and differ only on which property constitutes ''life,'' so adjudication turns on the definition itself; only evidence that all three criteria classify identically (a case of consecrated transmission, native acquisition, and practical function cleanly coming apart or together in a decisive way) could move the dispute off the definitional plane.',
    'Determines which arrangement''s epsilon governs any future classification of ''Hebrew''s linguistic life'' — this custody arrangement, the nativists'' acquisition arrangement, or the functionalists'' medium arrangement — as three separate stories rather than one hedged average.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary_dispute, conceptual, 'Location of the structural disagreement among the three sibling aliveness criteria.').

omega_variable(
    desecration_claim_routing,
    'Does the secular-national vernacularization of the register constitute appropriation that injures the tradition (this reading''s desecration charge against the revival project), or does sanctity attach to usage-context such that no persisting entity is injured when the corpus circulates unconsecrated?',
    'Trace concrete custody-transfer events — academy rulings on secular Hebrew print, liturgical formulas redeployed in state ceremony and advertising, prayer-language recycled for mundane purposes — and test each against the tradition''s own prohibition categories; the verdict is the tradition''s to give, which is why the charge routes through an omega rather than into this story''s extraction accounting.',
    'If desecration is confirmed by the tradition''s own lights, the injury generates a SEPARATE constraint story — this reading''s indictment of the vernacular regime, with its own victims and epsilon — and this file''s accounting remains about the custody arrangement itself; nothing here is revised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desecration_claim_routing, conceptual, 'Whether the vernacular revival counts as injury to the tradition, and which story carries that injury.').

omega_variable(
    consent_vs_identity_lock_composition,
    'At the interval''s end, how much of the custody arrangement''s persistence among its holders reflects renewed voluntary covenantal commitment versus identity-lock that credible exit options would dissolve?',
    'Longitudinal study of exit trajectories among those raised inside who leave: if costs and distress persist long after external barriers fall, lock dominates the residual suppression; if leavers report rapid relief and flourishing, consent dominates among stayers by contrast.',
    'Splits the residual suppression between genuine devotion (consent-shaped coordination) and fused identity (lock-shaped persistence), sharpening per-seat classification of the remaining community and testing the identity_coordination floor assumption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_identity_lock_composition, empirical, 'Voluntary renewal versus identity-fusion among current holders of the chain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 200, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(heb_liturg_pres_tr_t200, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t200, observed).
narrative_ontology:measurement(heb_liturg_pres_tr_t600, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 600, 0.12).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t600, observed).
narrative_ontology:measurement(heb_liturg_pres_tr_t1000, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1000, 0.14).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t1000, observed).
narrative_ontology:measurement(heb_liturg_pres_tr_t1500, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1500, 0.16).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t1500, observed).
narrative_ontology:measurement(heb_liturg_pres_tr_t1800, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1800, 0.18).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t1800, observed).
narrative_ontology:measurement(heb_liturg_pres_tr_t1880, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1880, 0.2).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t1880, observed).
narrative_ontology:measurement(heb_liturg_pres_tr_t1948, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t1948, observed).
narrative_ontology:measurement(heb_liturg_pres_tr_t2020, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement_basis(heb_liturg_pres_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(heb_liturg_pres_be_t200, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 200, 0.28).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t200, observed).
narrative_ontology:measurement(heb_liturg_pres_be_t600, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 600, 0.33).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t600, observed).
narrative_ontology:measurement(heb_liturg_pres_be_t1000, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1000, 0.36).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t1000, observed).
narrative_ontology:measurement(heb_liturg_pres_be_t1500, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t1500, observed).
narrative_ontology:measurement(heb_liturg_pres_be_t1800, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1800, 0.45).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t1800, observed).
narrative_ontology:measurement(heb_liturg_pres_be_t1880, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1880, 0.47).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t1880, observed).
narrative_ontology:measurement(heb_liturg_pres_be_t1948, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 1948, 0.44).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t1948, observed).
narrative_ontology:measurement(heb_liturg_pres_be_t2020, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement_basis(heb_liturg_pres_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(heb_liturg_pres_su_t200, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 200, 0.35).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t200, observed).
narrative_ontology:measurement(heb_liturg_pres_su_t600, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 600, 0.45).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t600, observed).
narrative_ontology:measurement(heb_liturg_pres_su_t1000, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1000, 0.52).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t1000, observed).
narrative_ontology:measurement(heb_liturg_pres_su_t1500, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t1500, observed).
narrative_ontology:measurement(heb_liturg_pres_su_t1800, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1800, 0.72).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t1800, observed).
narrative_ontology:measurement(heb_liturg_pres_su_t1880, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1880, 0.7).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t1880, observed).
narrative_ontology:measurement(heb_liturg_pres_su_t1948, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 1948, 0.48).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t1948, observed).
narrative_ontology:measurement(heb_liturg_pres_su_t2020, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement_basis(heb_liturg_pres_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'whether Hebrew's linguistic life ever lapsed' decomposes into three structurally distinct claims with different epsilons, victim sets, and research communities: this file (liturgical_preservation_reading: life subsists in consecrated transmission), native_generational_reading (life requires native child acquisition), and marketplace_pidgin_reading (life is practical inter-communal function). Each holds a single stable epsilon over the same historical record; they are linked here as a constraint family, not merged. Upstream relationship: this reading supplies the corpus-substrate and continuity claim that both siblings argue against or build upon, so its edges point to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
