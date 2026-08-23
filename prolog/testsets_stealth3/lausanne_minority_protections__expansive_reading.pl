% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__expansive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: lausanne_minority_protections__expansive_reading
 *   human_readable: Lausanne Functional Continuity Guarantee (Expansive Reading)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the expansive reading of the 1923 Treaty of
 *   Lausanne's minority-protection sections: that Articles 37-45 constitute
 *   fundamental law guaranteeing the functional continuity of pre-1923
 *   religious governance — institutional self-administration, communal
 *   property, and clergy formation — not merely individual worship. The
 *   standing arrangement the story is about is the guarantee as actually
 *   operated: entrenched on paper, administered day-to-day by the Turkish
 *   state through the General Directorate of Foundations, honored in the
 *   routine functioning of minority schools, hospitals, and cemeteries, and
 *   violated in waves (the 1942 capital levy, the 1955 pogrom, the 1964
 *   expulsions, the 1971 closure of the Halki Theological School, the 1974
 *   bar on minority foundations acquiring property), with a partial
 *   correction wave during the EU-accession reform period (2003-2011 property
 *   returns). Assessed by this reading's own lights, the arrangement is
 *   genuinely protective coordination that has repeatedly failed to protect,
 *   and whose survival now depends on which way the interpretive contest
 *   resolves. The claim and the metrics are authored independently: the claim
 *   is rope (a protective guarantee with a real coordination function and no
 *   rent-collector); the metrics describe a century of contested operation
 *   with high resistance and moderate residual extraction. Divergence between
 *   claim and computed classification is the datum, not an error to
 *   reconcile.
 *
 * KEY AGENTS:
 *   - - turkish_state: agenda-setter and administering power (institutional/constrained) — implements, constrains, and periodically violates the guarantee; bears its compliance costs
 *   - - greek_orthodox_minority_institutions: primary protected party (organized/identity_locked) — the Ecumenical Patriarchate complex; institutional continuity is the guarantee's core object
 *   - - armenian_apostolic_institutions: protected party (organized/identity_locked) — patriarchate and foundation network with deep property exposure
 *   - - jewish_community_institutions: protected party (moderate/constrained) — rabbinate and foundations with a thinner institutional base and heavier emigration
 *   - - greek_state_reciprocal_party: mirror-obligation holder and monitor (institutional/mobile) — bound symmetrically toward Western Thrace, watches Istanbul as signal
 *   - - unrecognized_religious_communities: excluded outsiders (powerless/mobile) — Alevi, Catholic, Protestant, Syriac communities outside the guarantee's 1923 categories
 *   - - european_union_institutions: external observer (institutional/analytical) — periodic benchmark pressure during accession windows
 *   - - generating analyst: analytical observer seat — sees the full interpretive contest across the three readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__expansive_reading, 0.25).
domain_priors:suppression_score(lausanne_minority_protections__expansive_reading, 0.46).
domain_priors:theater_ratio(lausanne_minority_protections__expansive_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(lausanne_minority_protections__expansive_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__expansive_reading, rope).
narrative_ontology:human_readable(lausanne_minority_protections__expansive_reading, "Lausanne Functional Continuity Guarantee (Expansive Reading)").
narrative_ontology:topic_domain(lausanne_minority_protections__expansive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__expansive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__expansive_reading, '6f876c33-0a5f-42d8-85fe-14d064352c77').
narrative_ontology:cs_kernel_codification('6f876c33-0a5f-42d8-85fe-14d064352c77', fixed_text).
narrative_ontology:cs_authority_grounding('6f876c33-0a5f-42d8-85fe-14d064352c77', lineage).
narrative_ontology:cs_reading_relation('6f876c33-0a5f-42d8-85fe-14d064352c77', lausanne_minority_protections__restrictive_reading, forecloses).
narrative_ontology:cs_reading_relation('6f876c33-0a5f-42d8-85fe-14d064352c77', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('6f876c33-0a5f-42d8-85fe-14d064352c77', foundational, institutional_continuity_treaty_guaranteed).
narrative_ontology:cs_axiom_status(institutional_continuity_treaty_guaranteed, holdable).
narrative_ontology:cs_axiom_grounding('6f876c33-0a5f-42d8-85fe-14d064352c77', institutional_continuity_treaty_guaranteed, conventional).
narrative_ontology:cs_axiom('6f876c33-0a5f-42d8-85fe-14d064352c77', secondary, clergy_formation_within_protected_scope).
narrative_ontology:cs_axiom_status(clergy_formation_within_protected_scope, holdable).
narrative_ontology:cs_axiom_grounding('6f876c33-0a5f-42d8-85fe-14d064352c77', clergy_formation_within_protected_scope, conventional).
narrative_ontology:cs_reference_frame('6f876c33-0a5f-42d8-85fe-14d064352c77', functional_continuity_fundamental_law).
narrative_ontology:cs_drift_state('6f876c33-0a5f-42d8-85fe-14d064352c77', contemporary_implementation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6f876c33-0a5f-42d8-85fe-14d064352c77', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__expansive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, greek_orthodox_minority_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, armenian_apostolic_institutions).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, jewish_community_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__expansive_reading, greek_state_reciprocal_party).
narrative_ontology:constraint_victim(lausanne_minority_protections__expansive_reading, turkish_state).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, lausanne_fundamental_law_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__expansive_reading, minority_rights_treaty_internationalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signatory and administering power. Implements the 1923 guarantees through the General Directorate of Foundations, which supervises minority foundation boards, approves property transactions, and licenses minority schools. Has alternated between accommodation (the 2008-2011 property-return decrees) and restriction (the 1971 closure of the Halki Theological School, the 1964 expulsion of Greek nationals, the 1974 ruling barring minority foundations from acquiring property). Formal withdrawal from the treaty carries severe international consequences, but day-to-day implementation discretion is wide, and forfeited communal properties accrued to the treasury for decades before partial restitution.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, turkish_state, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__expansive_reading, turkish_state, payer).

% The Ecumenical Patriarchate and its affiliated foundations in Istanbul: churches, cemeteries, orphanages, and schools operated under the treaty's institutional guarantees. Clergy formation depends on the Halki Theological School, closed by state decision since 1971, so ordinands train abroad and the pipeline narrows yearly. Community numbers fell from roughly 110,000 in the early 1950s to a few thousand today. Relocating the Patriarchate abroad has been discussed for decades and rejected each time, because the See's standing is inseparable from its historic seat.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, greek_orthodox_minority_institutions, beneficiary,
    organized, civilizational, identity_locked, national).

% The Armenian Patriarchate of Constantinople and its network of foundations, churches, and schools, operating hospitals and care homes that serve the wider public as well as the community. Leadership successions and board elections have repeatedly required state approval. Several dozen foundation properties remain unrestituted or in litigation after mid-century seizures. A large diaspora exists as demographic backstop, but the Istanbul institution is the community's historic center and cannot be moved without losing what it is.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, armenian_apostolic_institutions, beneficiary,
    organized, generational, identity_locked, national).

% The office of the Chief Rabbinate and associated foundations serving a community that numbered about 80,000 in 1927 and roughly 15,000 today. Runs schools, a hospital, and synagogues under the same guarantees. Succession and schooling arrangements depend on state-licensed frameworks. Heavy emigration, largely to Israel, has thinned the institutional base faster than for the Christian communities, and planning horizons are correspondingly shorter.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, jewish_community_institutions, beneficiary,
    moderate, biographical, constrained, national).

% Bound by the mirror-image obligations of the same 1923 settlement toward the Muslim minority of Western Thrace. Monitors the treatment of Istanbul's Greeks as a leading indicator for its own minority's security, and periodically raises the Halki closure, foundation properties, and Patriarchate status in bilateral talks. Draws diplomatic leverage from the precedent but administers nothing on the other side of the Aegean, and can raise or shelve the file as relations dictate.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, greek_state_reciprocal_party, observer,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__expansive_reading, greek_state_reciprocal_party, beneficiary).

% Alevi, Catholic, Protestant, and Syriac communities fall outside the 1923 categories, which recognized only three non-Muslim groups. Several have petitioned for equivalent recognition of their places of worship and training facilities and been told the treaty frame does not extend to them. They are not bound by the guarantee and cannot join it; their recourse is general domestic associations law, with predictably thinner protection.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, unrecognized_religious_communities, excluded,
    powerless, biographical, mobile, national).

% Since the 1999 Helsinki summit the accession framework has made minority-foundation property returns, Halki reopening, and legal-personality reform recurring benchmarks in progress reports. Applied sustained pressure through screening reports and negotiating chapters during the reform window; attention receded when negotiations stalled, and several reform tracks stalled with it.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__expansive_reading, european_union_institutions, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__expansive_reading, turkish_state).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__expansive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes, once and centrally, the terms on which a religious minority's institutions continue to exist inside a consolidated nation-state — replacing decade-by-decade ad hoc bargaining between community and state, with its recurring escalation risk, with a fixed entrenched template both sides can plan around across generations.
% TRANSFER_FUNCTION: Moves discretionary control over minority religious institutions — foundation boards, school administration, clergy training, property disposition — out of ordinary state administration and into communal self-government; binds the communities in exchange to operate only through the recognized pre-1923 institutional forms. Historically it also anchored a population transfer: the guarantee was the price of exempting Istanbul's Greeks from the compulsory exchange that moved most other Anatolian Greeks.
% ABSENT_VOICES: The unrecognized communities (Alevi, Catholic, Protestant, Syriac) would object that the guarantee's frozen 1923 categories protect their neighbors and abandon them on identical terrain; they sit outside the treaty frame entirely, petitioning through domestic channels. Lay members of the recognized communities would object that foundation boards have often been seated through state-supervised processes rather than communal election, leaving elite intermediaries to speak for them.
% DISAPPEARANCE_RATIONALE: If the guarantee vanished overnight, the remaining minority institutions lose their last legal shield: foundation assets revert to treasury-facing litigation paths, the Halki closure hardens into permanence with no reopening lever, clergy pipelines terminate within a generation, and the three communities' institutional survival collapses into ordinary minority associations law. The state would consolidate full administrative absorption domestically while paying a lasting international-credibility price; Greece would lose its monitoring signal and the Thrace mirror-obligations would destabilize in retaliation.
% FOUNDING_PROBLEM: After the Ottoman collapse, the Greco-Turkish war, and the compulsory population exchange, large minority populations remained stranded inside new nation-states constituted around a majority religion. The arrangement was built so that those states could be internationally recognized without either expelling or absorbing everyone outside the majority — securing basic communal continuity as the price of admission to the state system.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: European Commission accession progress reports have treated the obligations as outstanding benchmarks since 1998; United Nations treaty-body reviews and Council of Europe advisory-committee opinions attest continuing gaps in institutional autonomy, property, and clergy formation; and the Turkish foreign-ministry apparatus attests the obligations' continuing existence while disputing their scope — an adversarial attestation that itself confirms the problem has not died. No credible source outside the beneficiary set attests that the founding problem is resolved.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__expansive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__expansive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__expansive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__expansive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__expansive_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low-moderate (0.25 at interval end) because the guarantee collects no rents for anyone when honored; the residual reflects violation-wave residue — unrestituted properties, administrative subordination of foundation boards, and the still-open Halki closure, all of which the expansive reading indexes as extraction the guarantee was supposed to prevent. Suppression (0.46, a raw unscaled structural property — only extractiveness is scaled by directionality and scope in the engine) is composed almost entirely of exit-blockade rather than active coercion by the guarantee itself: the protected institutions cannot leave their identities behind (relocating the Patriarchate means dissolving it), and the state cannot formally denounce the treaty without severe international cost. The active coercive force in this system belongs to the state's suppression of minority institutional life, which loads onto resistance (0.66) — the documented hostility waves of 1942, 1955, 1964, 1971, and 1974. Accessibility collapse is moderate (0.40): mass emigration proved that leaving remained physically possible, so alternatives did not fully collapse; but for those who stayed, the in-place alternative (equal civic integration without communal institutions) was politically unavailable. Theater ratio 0.28: the foundations genuinely function daily, but a growing share of activity around the guarantee is commemorative and diplomatic performance. The temporal series run on one shared eleven-point grid (t=0 to t=100, decade steps mapping 1923-2023) so every tracked metric is authored at every examined time point; the series show one full enforcement-capacity cycle — rising defensive intensity through the mid-century violation waves, peaking around t=60, easing through the EU-reform window, ticking back up with post-2013 stagnation. The oscillation is driven by external factors (state-hostility phases, EU conditionality windows, the 1974 geopolitical rupture), not by intermittent reinforcement operated by the guarantee itself.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different classifications from the same structural data. From the turkish_state seat, the arrangement presents as imposed obligation: sovereignty over minority institutional life conceded at the founding moment, costs without counterpart, administered grudgingly — a constraint experienced as constraint. From the three protected seats, the same arrangement is the thin legal line between communal continuity and absorption — a lifeline whose failures are catastrophic rather than burdensome. From the excluded communities' seat, the arrangement is an arbitrary boundary-drawing that protects insiders and abandons neighbors on identical terrain. From the EU observer seat, it is a benchmark instrument whose value lies in its gaps. None of these is the story-level truth; the engine computes the divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The three declared beneficiary groups derive low directionality (near the subsidy end) — the guarantee operates for them, though their identity_locked exits mean they cannot arbitrage away its failures, which is why the beneficiary seats still experience real harm when it fails. The turkish_state is deliberately NOT placed in the beneficiaries or victims arrays: declaring it a victim would import the restrictive sibling's framing (state-as-injured-party) into this reading's structural data, and declaring it a beneficiary would misread its incidental stability gains as the reason the arrangement exists. Its cost-bearing is carried by the stakeholder layer (role: agenda_setter with secondary_role payer) and by this commentary. No directionality_overrides are authored: the override mechanism keys on the power atom, and an override set for institutional actors would smear identically across the state, Greece, and the EU institutions, whose relationships to this guarantee differ sharply. The derivation chain is trusted here; the residual per-agent differentiation lives in the stakeholder situations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing communal continuity for populations stranded inside consolidating nation-states — remains live: the protected communities are demographically marginal, the seminary is still closed, and property litigation continues. Declaring the mandate resolved (on a civic-equality-has-arrived theory) would misclassify a living lifeline as vestigial; the framework's mismatch consumer would correctly flag that as premature obsolescence. Conversely, reading every administrative restriction as bad-faith extraction mistakes a genuine coordination achievement — a century of schools, hospitals, and cemeteries run under a fixed legal template — for a pure snare. The classification guards both errors: the coordination function is real, no seat collects rents, and the mandate has not outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_adjudication_locus,
    'Who adjudicates the kernel''s scope — the administering state''s domestic institutions alone, or international review bodies alongside them?',
    'Track the divergence between European Court of Human Rights and UN treaty-body jurisprudence on minority foundations, seminary reopening, and legal personality versus domestic court and administrative practice over successive review cycles.',
    'If domestic-only adjudication consolidates, this reading''s operation decays toward the restrictive sibling''s practical outcome — a paper guarantee with rising theater_ratio and deepening institutional vulnerability; internationalized adjudication stabilizes the protective profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_adjudication_locus, conceptual, 'Locus of authoritative interpretation determines whether the expansive reading stays operative.').

omega_variable(
    reading_indexed_epsilon,
    'This story''s epsilon is indexed to the expansive reading''s own lights over the standing arrangement; sibling readings author different epsilon values over the same referent. How should cross-reading comparison handle this?',
    'Family-level meta-analysis must carry reading identity alongside every epsilon value and refuse pooled averaging across siblings; the referent (the standing arrangement of treaty-plus-administration) is shared, the valuation is not.',
    'Prevents false convergence verdicts: a low epsilon authored under restrictive lights and a moderate epsilon here do not contradict each other — they measure the same arrangement through different committer frames.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexed_epsilon, conceptual, 'Epsilon is reading-indexed; cross-sibling pooling would fabricate disagreement or agreement.').

omega_variable(
    reciprocity_deterrence_dependence,
    'Does the guarantee''s persistence rest on bilateral reciprocity with the mirror-image Greek obligations toward Western Thrace, rather than on legal force?',
    'Compare the timing of violation waves against reciprocity shocks (e.g., the post-1974 Cyprus rupture accelerating pressure on Istanbul''s Greek community) and against periods when bilateral relations warmed.',
    'If deterrence-dependent, the suppression_requirement series understates fragility — the guarantee''s stability is geopolitical, not juridical, and a reciprocity break could collapse protection faster than any legal analysis predicts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_deterrence_dependence, empirical, 'Whether continuity is held by treaty law or by bilateral deterrence.').

omega_variable(
    frozen_category_exclusion_costs,
    'Do the fixed 1923 recognition categories impose real costs — on the excluded communities denied entry into the guarantee, and on recognized communities locked into governance forms frozen at 1923?',
    'Survey litigation and petition outcomes of Alevi, Catholic, Protestant, and Syriac communities seeking equivalent recognition, plus longitudinal comparison of institutional vitality between recognized and unrecognized communities.',
    'Material exclusion costs would add a paying seat the current authoring deliberately lacks, raising computed asymmetry and warranting a revised story with explicit victim declarations; immaterial costs confirm the protective profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frozen_category_exclusion_costs, empirical, 'Hidden cost side of the guarantee''s frozen-category architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__expansive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_expansive_tr_t0, lausanne_minority_protections__expansive_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t0, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t10, lausanne_minority_protections__expansive_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t10, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t20, lausanne_minority_protections__expansive_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t20, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t30, lausanne_minority_protections__expansive_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t30, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t40, lausanne_minority_protections__expansive_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t40, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t50, lausanne_minority_protections__expansive_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t50, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t60, lausanne_minority_protections__expansive_reading, theater_ratio, 60, 0.39).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t60, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t70, lausanne_minority_protections__expansive_reading, theater_ratio, 70, 0.35).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t70, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t80, lausanne_minority_protections__expansive_reading, theater_ratio, 80, 0.24).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t80, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t90, lausanne_minority_protections__expansive_reading, theater_ratio, 90, 0.19).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t90, observed).
narrative_ontology:measurement(lausanne_expansive_tr_t100, lausanne_minority_protections__expansive_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(lausanne_expansive_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(lausanne_expansive_be_t0, lausanne_minority_protections__expansive_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(lausanne_expansive_be_t0, observed).
narrative_ontology:measurement(lausanne_expansive_be_t10, lausanne_minority_protections__expansive_reading, base_extractiveness, 10, 0.09).
narrative_ontology:measurement_basis(lausanne_expansive_be_t10, observed).
narrative_ontology:measurement(lausanne_expansive_be_t20, lausanne_minority_protections__expansive_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement_basis(lausanne_expansive_be_t20, observed).
narrative_ontology:measurement(lausanne_expansive_be_t30, lausanne_minority_protections__expansive_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(lausanne_expansive_be_t30, observed).
narrative_ontology:measurement(lausanne_expansive_be_t40, lausanne_minority_protections__expansive_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement_basis(lausanne_expansive_be_t40, observed).
narrative_ontology:measurement(lausanne_expansive_be_t50, lausanne_minority_protections__expansive_reading, base_extractiveness, 50, 0.37).
narrative_ontology:measurement_basis(lausanne_expansive_be_t50, observed).
narrative_ontology:measurement(lausanne_expansive_be_t60, lausanne_minority_protections__expansive_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement_basis(lausanne_expansive_be_t60, observed).
narrative_ontology:measurement(lausanne_expansive_be_t70, lausanne_minority_protections__expansive_reading, base_extractiveness, 70, 0.33).
narrative_ontology:measurement_basis(lausanne_expansive_be_t70, observed).
narrative_ontology:measurement(lausanne_expansive_be_t80, lausanne_minority_protections__expansive_reading, base_extractiveness, 80, 0.29).
narrative_ontology:measurement_basis(lausanne_expansive_be_t80, observed).
narrative_ontology:measurement(lausanne_expansive_be_t90, lausanne_minority_protections__expansive_reading, base_extractiveness, 90, 0.23).
narrative_ontology:measurement_basis(lausanne_expansive_be_t90, observed).
narrative_ontology:measurement(lausanne_expansive_be_t100, lausanne_minority_protections__expansive_reading, base_extractiveness, 100, 0.25).
narrative_ontology:measurement_basis(lausanne_expansive_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_expansive_su_t0, lausanne_minority_protections__expansive_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(lausanne_expansive_su_t0, observed).
narrative_ontology:measurement(lausanne_expansive_su_t10, lausanne_minority_protections__expansive_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement_basis(lausanne_expansive_su_t10, observed).
narrative_ontology:measurement(lausanne_expansive_su_t20, lausanne_minority_protections__expansive_reading, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(lausanne_expansive_su_t20, observed).
narrative_ontology:measurement(lausanne_expansive_su_t30, lausanne_minority_protections__expansive_reading, suppression_requirement, 30, 0.38).
narrative_ontology:measurement_basis(lausanne_expansive_su_t30, observed).
narrative_ontology:measurement(lausanne_expansive_su_t40, lausanne_minority_protections__expansive_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(lausanne_expansive_su_t40, observed).
narrative_ontology:measurement(lausanne_expansive_su_t50, lausanne_minority_protections__expansive_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(lausanne_expansive_su_t50, observed).
narrative_ontology:measurement(lausanne_expansive_su_t60, lausanne_minority_protections__expansive_reading, suppression_requirement, 60, 0.61).
narrative_ontology:measurement_basis(lausanne_expansive_su_t60, observed).
narrative_ontology:measurement(lausanne_expansive_su_t70, lausanne_minority_protections__expansive_reading, suppression_requirement, 70, 0.57).
narrative_ontology:measurement_basis(lausanne_expansive_su_t70, observed).
narrative_ontology:measurement(lausanne_expansive_su_t80, lausanne_minority_protections__expansive_reading, suppression_requirement, 80, 0.47).
narrative_ontology:measurement_basis(lausanne_expansive_su_t80, observed).
narrative_ontology:measurement(lausanne_expansive_su_t90, lausanne_minority_protections__expansive_reading, suppression_requirement, 90, 0.41).
narrative_ontology:measurement_basis(lausanne_expansive_su_t90, observed).
narrative_ontology:measurement(lausanne_expansive_su_t100, lausanne_minority_protections__expansive_reading, suppression_requirement, 100, 0.46).
narrative_ontology:measurement_basis(lausanne_expansive_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__expansive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__expansive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition: the colloquial label 'Lausanne minority protections' covers three structurally distinct claims that cannot share one epsilon — scope (this file, expansive_reading: what is guaranteed), enforcement channel (guarantor_reading: who makes it stick), and minimal scope (restrictive_reading: worship only, the rest domestic). This story links both siblings. Structural relations: the expansive reading's widened protected surface changes the guarantor reading's operating environment (more to supervise, higher stakes of supervision) without ruling it out; the expansive and restrictive readings allocate the same treaty articles' coverage incompatibly, so no single legal framework can hold both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
