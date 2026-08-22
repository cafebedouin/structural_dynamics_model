% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Market-Licensing Reading — Categorical Market-Harm Bar
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   The fair use kernel — 17 U.S.C. §107 and its Folsom v. Marsh ancestry —
 *   supports competing readings. This story instantiates the
 *   market_licensing_reading: the rule that any use for which a licensing
 *   mechanism exists (or could be constructed) inflicts cognizable market
 *   harm, confining fair use to uses for which no market exists. Under this
 *   reading the doctrine collapses toward nullity in any monetizable context:
 *   every quotation, clip, sample, or excerpt sits inside someone's potential
 *   license inventory. The ε referent is the standing arrangement under
 *   contest — fair use as construed by this reading — assessed on this
 *   reading's own terms; the endorsed alternatives (transformative-right and
 *   narrow-defense constructions) are separate stories, not hedged averages
 *   here. Per the ε-invariance principle the kernel decomposes into three
 *   linked constraint stories: this one (categorical market-harm bar, highest
 *   ε), transformative_right_reading (transformativeness trumps licensable
 *   markets), and narrow_defense_reading (affirmative-defense framing,
 *   intermediate). Each carries its own beneficiaries, victims, and type;
 *   network edges link the family. Claim and metrics are authored
 *   independently: the reading presents itself as faithful property
 *   enforcement, while the authored metrics describe an actively enforced,
 *   high-transfer arrangement with a growing share of ceremonial four-factor
 *   balancing.
 *
 * KEY AGENTS:
 *   - rights_holder_publishers: Agenda-setting beneficiary (institutional/arbitrage) — funds the reading's litigation and lobbying; collects fees, settlements, and statutory damages
 *   - licensing_intermediaries: Pure beneficiary (organized/mobile) — clearance houses and collecting societies taking commissions on every drawn-in use category
 *   - courts_judiciary: Agenda-setting administrator (institutional/analytical) — administers the four-factor test; collects nothing, bears nothing
 *   - secondary_creators_remixers: Primary payer (powerless/identity_locked) — fan artists, video essayists, remix musicians; exit means ceasing the practice
 *   - documentary_filmmakers: Payer (moderate/constrained) — archival-dependent nonfiction; clearance quotes exceed budgets
 *   - educational_institutions: Payer (organized/constrained) — course materials and streaming under clearance-office control
 *   - research_archives_libraries: Payer (organized/constrained) — mass digitization and preservation stalled by per-item clearance demands
 *   - prospective_audiences_of_uncleared_works: Excluded (powerless/trapped) — audiences of works never made; no seat in any negotiation
 *   - fair_use_scholars_public_interest_counsel: Observer (organized/analytical) — test-case litigation, amicus work, empirical scholarship
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.88).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.83).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, snare).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Market-Licensing Reading — Categorical Market-Harm Bar").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'ceef38da-66cb-4789-9b38-0df2b79e6f1d').
narrative_ontology:cs_kernel_codification('ceef38da-66cb-4789-9b38-0df2b79e6f1d', fixed_text).
narrative_ontology:cs_authority_grounding('ceef38da-66cb-4789-9b38-0df2b79e6f1d', lineage).
narrative_ontology:cs_interpretation_layer_present('ceef38da-66cb-4789-9b38-0df2b79e6f1d').
narrative_ontology:cs_reading_relation('ceef38da-66cb-4789-9b38-0df2b79e6f1d', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_reading_relation('ceef38da-66cb-4789-9b38-0df2b79e6f1d', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_axiom('ceef38da-66cb-4789-9b38-0df2b79e6f1d', foundational, licensability_defeats_fair_use).
narrative_ontology:cs_axiom_status(licensability_defeats_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('ceef38da-66cb-4789-9b38-0df2b79e6f1d', licensability_defeats_fair_use, empirically_contingent).
narrative_ontology:cs_axiom('ceef38da-66cb-4789-9b38-0df2b79e6f1d', secondary, market_absence_bounds_fair_use).
narrative_ontology:cs_axiom_status(market_absence_bounds_fair_use, holdable).
narrative_ontology:cs_axiom_grounding('ceef38da-66cb-4789-9b38-0df2b79e6f1d', market_absence_bounds_fair_use, conventional).
narrative_ontology:cs_reference_frame('ceef38da-66cb-4789-9b38-0df2b79e6f1d', market_completeness_property_frame).
narrative_ontology:cs_drift_state('ceef38da-66cb-4789-9b38-0df2b79e6f1d', post_warhol_google_books_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ceef38da-66cb-4789-9b38-0df2b79e6f1d', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rights_holder_publishers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, secondary_creators_remixers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, research_archives_libraries).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, property_theory_of_copyright).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, market_harm_primacy).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, licensing_market_completeness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major publishers, studios, labels, and image libraries own large catalogs and fund the litigation and lobbying that advance the rule that any licensable use infringes. They send demand letters, file suits, operate licensing desks, and collect fees, settlements, and statutory damages. Their exit is easy: catalogs can be repriced, cross-licensed, or moved between enforcement strategies and jurisdictions.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rights_holder_publishers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, rights_holder_publishers, beneficiary).

% Clearance houses, collecting societies, and licensing platforms broker permissions between owners and users, taking commissions on each transaction. Their business volume grows with every category of use drawn into the licensing market. They can pivot to new catalog types or territories if a category closes.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, licensing_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Federal courts administer the four-factor test and decide which uses escape liability. Their precedents set the rule's reach; they collect nothing and bear nothing directly. They can reshape the doctrine case by case but face institutional constraints of precedent and docket.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Fan artists, video essayists, remix musicians, and reaction creators build work out of existing recordings and footage. A takedown or demand letter typically ends the project; per-use licenses are impossible at their volumes; abandoning reuse means abandoning the practice that defines their output. Many self-censor before any notice arrives.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, secondary_creators_remixers, payer,
    powerless, biographical, identity_locked, global).

% Nonfiction filmmakers need archival footage, music, and news clips. Clearance quotes for a single documentary can exceed the entire budget, so films are shelved, recut, or narrated around gaps. There is no substitute input: the film is about the archive.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, continental).

% Universities and school systems post readings, stream films, and build course packs. Clearance offices negotiate site licenses; ambiguous uses are dropped from syllabi after counsel review. Their scale gives bargaining leverage, but their mission requires broad access they cannot individually price.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educational_institutions, payer,
    organized, generational, constrained, national).

% Libraries and archives digitize, preserve, and provide access to collections containing third-party rights. Mass-digitization projects stall when per-item clearance is demanded. Governing statutes give partial shelter, but the rule narrows what counts as preservation versus unlicensed distribution.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, research_archives_libraries, payer,
    organized, generational, constrained, national).

% Readers, viewers, and future scholars who never see the biographies, documentaries, and critical editions abandoned over clearance costs. They have no seat in any negotiation; their loss is invisible because the work never exists to miss.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, prospective_audiences_of_uncleared_works, excluded,
    powerless, generational, trapped, global).

% Academic commentators, clinic lawyers, and advocacy organizations litigate test cases, file amicus briefs, and publish empirical work on licensing and substitution. They shape the interpretive environment but collect no fees and bear no clearance costs.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, fair_use_scholars_public_interest_counsel, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, rights_holder_publishers).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a permission market connecting owners of large catalogs with commercial users: standardized clearance, pricing, and dispute channels solve once what bilateral negotiation would solve badly. The bright-line rule also purports to reduce doctrinal uncertainty — users know to license, owners know what to charge.
% TRANSFER_FUNCTION: Moves licensing fees, settlement payments, and statutory damages from anyone whose use could plausibly be licensed to rights holders and their intermediaries; secondarily moves creative output itself, as works are abandoned, recut, or never begun to avoid clearance exposure.
% ABSENT_VOICES: Prospective audiences of uncleared works — readers, viewers, and future scholars of biographies, documentaries, and editions abandoned over clearance costs — have no seat anywhere in the process. Individual creators facing demand letters rarely answer through counsel; the public-domain side of the ledger is represented by no party in licensing negotiations.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, licensing demand in currently-fair categories would collapse as users relied on the statutory defense; documentary and archival clearance costs would drop sharply; secondary creation in sampled and quoted forms would expand; the demand-letter and filtering industries built on the rule would shrink. Rights-holder revenues from marginal-use licensing would fall — the rearrangement is precisely what the benefiting parties litigate to prevent.
% FOUNDING_PROBLEM: The four-factor test's open texture left both sides exposed: users could not predict which unauthorized uses were lawful, and rights holders watched marginal uses escape compensation. The market-licensing reading was built to close that gap by making licensability itself the test.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Judge Pierre Leval's 'Toward a Fair Use Standard' (1990) and the subsequent transformative-use scholarly consensus attest that the founding problem was answered by refocusing the doctrine on transformation rather than market presence; the Library Copyright Alliance, the Society of American Archivists, and the Documentary Filmmakers' Statement of Best Practices attest from the user side that the problem as framed (incentive erosion requiring categorical market-harm primacy) is misdescribed. No source inside the rights-holder set is offered as corroboration — self-attestation is excluded by design.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.88 (interval end) because the rule converts every licensable context into a payable one and the licensing universe expands to meet it — the expected structural delta for this reading. Suppression (0.83) is structural: statutory damages up to $150,000 per work for willful infringement, litigation-cost asymmetry, and automated takedown make the unlicensed route prohibitively risky even where a court might ultimately excuse the use; suppression is authored as a raw structural property and is not scaled by power or scope — the engine owns any scaling. Theater_ratio (0.45) reflects the four-factor ceremony: opinions continue to weigh purpose, nature, amount, and effect while the market-presence question effectively decides outcomes, and incentive-preservation rhetoric is performed over what functions as clearance enforcement. Accessibility_collapse (0.70): once the rule is understood, alternatives collapse for monetizable contexts, but residual space remains — private and non-monetizable uses, statutorily sheltered library activities, and uses beyond US enforcement reach. Resistance (0.60): Campbell v. Acuff-Rose, Authors Guild v. Google, Warhol dissenters, library and filmmaker coalitions, and best-practices statements mount sustained pushback that has repeatedly blunted the reading at the doctrinal layer even as private enforcement advanced it. The three measurement series share one time grid ({0, 8, 16, 24, 32, 40}); all points are observed. The trajectories are monotonic rather than cyclical: a mid-interval judicial counter-current (Campbell, roughly t=9–16) temporarily flattened base_extractiveness growth, but platform-side enforcement (DMCA notice volumes, automated content identification from roughly t=22 onward) bypassed the doctrinal relief, so the series resume climbing rather than oscillating — the cycle-breaker is enforcement-layer substitution, documented in the private_enforcement_attribution omega.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the rights-holder seat the arrangement reads as a functioning market it built and staffed: licenses clear, fees flow, disputes settle — a coordination surface with overhead. From the secondary-creator and archive seats the same surface operates as a toll gate with no bypass: the defense the statute promises is unavailable precisely where their work lives. Courts occupy an analytic middle: they administer a balancing test whose factors they weight differently case by case, which is why the same doctrine produces both Google Books (reading defeated) and Warhol (reading advanced) outcomes. Coalition capacity partially differentiates same-power payer seats: organized institutions (universities, libraries) extracted partial carve-outs through collective action and best-practices statements, while powerless individual creators received none — power parity on paper diverges into different effective positions through coalition formation. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights holders sit near the beneficiary pole: the rule subsidizes their catalogs by converting potential unlicensed uses into fee events, and their arbitrage-grade exit (repricing, cross-licensing, forum selection) insulates them from the rule's costs. Licensing intermediaries likewise collect without bearing. Secondary creators, documentary filmmakers, universities, and archives sit near the target pole: they pay per-use or abandon work, and their exits are constrained or identity-bound — a remixer's exit is ceasing to remix, an identity-fusion mechanism in which the creative practice is constituted through appropriation of existing works, so the classification would change materially if that identity frame broke (a remixer who reframes as original-only creator exits the target seat entirely). Courts and scholars hold analytic seats with no material flow. The beneficiary and victim declarations in base_properties feed the derivation; no directionality overrides were needed because the beneficiary/victim structure plus exit options already separate the poles cleanly.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline matters here in both directions. Reading the arrangement as pure coordination (a licensing market enabling access) would launder the extraction: the licensing market's volume is manufactured by closing the unlicensed alternative, so the coordination is downstream of the suppression, not independent of it. Reading it as degraded inertia would mislocate the gains: extraction is not diffusely borne with nobody capturing — named seats collect it, which is disqualifying for the inertia pattern. The snare claim keeps both errors visible: coordination story present but derivative, enforcement load-bearing, victims named. On the genealogy side, the founding problem (open-textured uncertainty about unauthorized use) is contested rather than dead, so no mandatrophy resolution is declared; the mismatch consumer should read the founding_problem_status × disappearance_verdict pair rather than the origin narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading of the fair_use_statutory_exception kernel; which specific structural element separates it from the transformative_right_reading and narrow_defense_reading siblings, and what would adopting a sibling change?',
    'Doctrinal analysis locating the dispute in whether licensability is dispositive of market harm: the transformative reading denies dispositive force (transformativeness can defeat market harm even where licensing exists); the narrow-defense reading accepts market-value preservation but frames fair use as a rebuttable defense rather than a categorical bar.',
    'If the transformative reading prevails, the victim set shrinks to non-transformative commercial uses and epsilon falls sharply; if the narrow-defense reading prevails, extraction persists but with case-by-case leakage; as authored here, the categorical rule leaves no leakage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committer structure: one reading of the fair use kernel; sibling readings relocate the victim set and epsilon.').

omega_variable(
    substitution_empirics,
    'Does unlicensed use actually displace sales of licensed equivalents, or do the two serve largely different demands?',
    'Econometric studies of music sampling, streaming-era licensing windows, Google Books circulation data, and natural experiments where fair use rulings opened categories.',
    'If substitution is weak, the causal premise of the market-harm rule fails, epsilon drops toward coordination-cost levels, and the reading loses its empirical footing; if strong, the measured transfer reflects real displacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_empirics, empirical, 'Whether the licensability-equals-harm premise survives contact with substitution data.').

omega_variable(
    private_enforcement_attribution,
    'Is the measured intensification a property of the doctrinal reading itself, or of the private enforcement stack (automated takedown, demand-letter mills, platform filtering) that exploits it?',
    'Compare extraction trajectories across jurisdictions with identical treaty obligations but different notice regimes; measure outcome changes when platforms adjust filter thresholds independently of case law.',
    'If the enforcement stack drives the curve, doctrinal reform alone under-fixes the arrangement and the constraint family boundary should be redrawn around the infrastructure; if doctrine drives it, the reading is the operative constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_enforcement_attribution, conceptual, 'Attribution of rising extraction between doctrine and enforcement infrastructure.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the measured suppression is structural (automated takedowns, litigation-cost asymmetry) versus internalized (clearance culture taught as professional norm, self-censorship before any threat arrives)?',
    'Post-rule-change suppression trajectory: if creator behavior relaxes after enforcement thresholds change without accompanying education campaigns, suppression was mostly structural; persistent caution indicates internalization.',
    'Internalized suppression outlives formal reform — removing the reading would not immediately restore the erased use categories, and measured accessibility_collapse would overstate current enforcement while understating durable chill.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between external enforcement machinery and professional self-censorship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mkt_licensing_reading_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(mkt_licensing_reading_tr_t0, observed).
narrative_ontology:measurement(mkt_licensing_reading_tr_t8, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(mkt_licensing_reading_tr_t8, observed).
narrative_ontology:measurement(mkt_licensing_reading_tr_t16, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement_basis(mkt_licensing_reading_tr_t16, observed).
narrative_ontology:measurement(mkt_licensing_reading_tr_t24, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(mkt_licensing_reading_tr_t24, observed).
narrative_ontology:measurement(mkt_licensing_reading_tr_t32, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(mkt_licensing_reading_tr_t32, observed).
narrative_ontology:measurement(mkt_licensing_reading_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement_basis(mkt_licensing_reading_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(mkt_licensing_reading_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(mkt_licensing_reading_be_t0, observed).
narrative_ontology:measurement(mkt_licensing_reading_be_t8, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(mkt_licensing_reading_be_t8, observed).
narrative_ontology:measurement(mkt_licensing_reading_be_t16, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement_basis(mkt_licensing_reading_be_t16, observed).
narrative_ontology:measurement(mkt_licensing_reading_be_t24, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement_basis(mkt_licensing_reading_be_t24, observed).
narrative_ontology:measurement(mkt_licensing_reading_be_t32, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement_basis(mkt_licensing_reading_be_t32, observed).
narrative_ontology:measurement(mkt_licensing_reading_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(mkt_licensing_reading_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(mkt_licensing_reading_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(mkt_licensing_reading_su_t0, observed).
narrative_ontology:measurement(mkt_licensing_reading_su_t8, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement_basis(mkt_licensing_reading_su_t8, observed).
narrative_ontology:measurement(mkt_licensing_reading_su_t16, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement_basis(mkt_licensing_reading_su_t16, observed).
narrative_ontology:measurement(mkt_licensing_reading_su_t24, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement_basis(mkt_licensing_reading_su_t24, observed).
narrative_ontology:measurement(mkt_licensing_reading_su_t32, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement_basis(mkt_licensing_reading_su_t32, observed).
narrative_ontology:measurement(mkt_licensing_reading_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.83).
narrative_ontology:measurement_basis(mkt_licensing_reading_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, dmca_notice_and_takedown_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' conflates three structurally distinct claims sharing one statutory kernel. This story (market_licensing_reading) authors the categorical version: licensability defeats the defense, leaving fair use confined to marketless uses — highest epsilon, victims spanning all monetizable reuse. transformative_right_reading authors the facilitation version: transformativeness defeats market harm even where licensing exists — lower epsilon, victim set shrinks to non-transformative uses. narrow_defense_reading authors the procedural version: fair use as narrowly construed affirmative defense — intermediate epsilon with case-by-case leakage. The upstream story (this one) supplies the property-theoretic premise the other two modify; each story links the others via network edges per the constraint-family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
