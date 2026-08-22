% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__judicial_ambiguity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__judicial_ambiguity_reading, []).

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
 *   constraint_id: copyright_constitutional_mandate__judicial_ambiguity_reading
 *   human_readable: Copyright Term Deference Doctrine (Judicial Ambiguity Reading of the Limited Times Clause)
 *   domain: legal/constitutional/political_economy
 *
 * SUMMARY:
 *   The Progress Clause empowers Congress to secure exclusive rights 'for
 *   limited Times.' Since 1962 Congress has extended copyright terms eleven
 *   times by ordinary legislation, culminating in the 1998 Copyright Term
 *   Extension Act (plus twenty years, retroactive; life-plus-seventy for
 *   individuals, ninety-five for corporate works). The judicial-ambiguity
 *   reading holds that 'limited Times' is a zone of legislative discretion
 *   policed only by rational basis review — the standing arrangement this
 *   story is about is that deference regime itself, as it operates from
 *   Eldred v. Ashcroft (2003) through Golan v. Holder (2012) to the present.
 *   Epsilon's referent is the deference arrangement under contest, assessed
 *   by this reading's own lights: the reading holds the textual ambiguity
 *   genuine and the comity function real, and it also sees each exercise of
 *   the discretion ratchet terms upward while the review standard never
 *   binds. This is one reading of the kernel
 *   copyright_constitutional_mandate; the sibling readings
 *   (public_scaffold_reading, corporate_enclosure_reading) are separate
 *   stories, decomposed per the epsilon-invariance principle because one
 *   clause supports structurally distinct constraints with distinct
 *   beneficiary/victim sets and distinct epsilon. Claim and metrics are
 *   independent authored facts: claimed_type states this reading's structural
 *   belief; the metric values state the arrangement's observed operation. KEY
 *   AGENTS (by structural relationship): congressional_legislature —
 *   agenda-setter and beneficiary (institutional/mobile), holds the insulated
 *   discretion; incumbent_copyright_holders — primary beneficiary
 *   (powerful/arbitrage), collects the extended-term revenue; federal_courts
 *   — administering agenda-setter with incidental benefit
 *   (institutional/constrained); public_domain_users — primary payer
 *   (organized/trapped); future_creators — payer (powerless/trapped);
 *   constitutional_challenge_litigants — payer whose forum the doctrine
 *   closed (moderate/trapped); constitutional_fixity — structural payer,
 *   non-agent; unorganized_public_domain_constituency — excluded seat
 *   (powerless/trapped); constitutional_scholars — analytical observer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.55).
domain_priors:suppression_score(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.68).
domain_priors:theater_ratio(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__judicial_ambiguity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__judicial_ambiguity_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__judicial_ambiguity_reading, "Copyright Term Deference Doctrine (Judicial Ambiguity Reading of the Limited Times Clause)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__judicial_ambiguity_reading, "legal/constitutional/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__judicial_ambiguity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__judicial_ambiguity_reading, '8112df75-4ea1-4832-bd54-878860eb4582').
narrative_ontology:cs_kernel_codification('8112df75-4ea1-4832-bd54-878860eb4582', fixed_text).
narrative_ontology:cs_authority_grounding('8112df75-4ea1-4832-bd54-878860eb4582', lineage).
narrative_ontology:cs_interpretation_layer_present('8112df75-4ea1-4832-bd54-878860eb4582').
narrative_ontology:cs_reading_relation('8112df75-4ea1-4832-bd54-878860eb4582', copyright_constitutional_mandate__corporate_enclosure_reading, influences).
narrative_ontology:cs_reading_relation('8112df75-4ea1-4832-bd54-878860eb4582', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('8112df75-4ea1-4832-bd54-878860eb4582', foundational, limited_times_political_question).
narrative_ontology:cs_axiom_status(limited_times_political_question, holdable).
narrative_ontology:cs_axiom_grounding('8112df75-4ea1-4832-bd54-878860eb4582', limited_times_political_question, conventional).
narrative_ontology:cs_axiom('8112df75-4ea1-4832-bd54-878860eb4582', secondary, rational_basis_suffices_for_term_legislation).
narrative_ontology:cs_axiom_status(rational_basis_suffices_for_term_legislation, holdable).
narrative_ontology:cs_axiom_grounding('8112df75-4ea1-4832-bd54-878860eb4582', rational_basis_suffices_for_term_legislation, conventional).
narrative_ontology:cs_reference_frame('8112df75-4ea1-4832-bd54-878860eb4582', legislative_discretion_comity).
narrative_ontology:cs_drift_state('8112df75-4ea1-4832-bd54-878860eb4582', post_eldred_golan_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8112df75-4ea1-4832-bd54-878860eb4582', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_legislature).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_users).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_challenge_litigants).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, rational_basis_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__judicial_ambiguity_reading, separation_of_powers_comity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and repeatedly extends copyright term length by ordinary legislation — eleven extensions since 1962, culminating in the 1998 act that added twenty years and applied them retroactively to existing works. The courts' deferential standard means each extension it passes stands without judicial invalidation. It receives the discretion itself, plus sustained support from the industries whose portfolios the extensions protect. It could revise or shorten terms at any time by the same ordinary process, which in practice it never does.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_legislature, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, congressional_legislature, beneficiary).

% Hold the extended terms: corporate works locked at ninety-five years, individual works at life-plus-seventy. Each extension cycle adds two decades of exclusive revenue to works nearing the public domain, and the deferential review standard is what keeps those extensions safe from challenge. Organized trade associations fund the extension campaigns and supply the reliance-interest and treaty-harmonization arguments. Their portfolios span jurisdictions with harmonized terms, so they trade across the gap between the constitutional text's 'limited' language and the doctrine's permissive reading.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Administer the review standard: hear term-extension challenges and resolve them under rational basis, which has never once invalidated an extension. The 2003 majority articulated the 'traditional contours' formulation while declining to apply it against the very act under review; the 2012 decision closed the First Amendment route as well. The standard buys the branch peace with Congress on an economic-policy question and spares it line-drawing the text gives no metric for. It cannot leave the docket — challenges keep arriving — but it chooses how hard to look.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__judicial_ambiguity_reading, federal_courts, beneficiary).

% Libraries, archives, museums, preservation projects, and derivative creators who would copy, adapt, and republish works whose terms keep extending. Every twenty-year extension freezes material they would otherwise have in hand; digitization projects are routinely abandoned or narrowed because the rights status of twentieth-century works is unresolvable. They are organized — library and archive associations filed briefs in the 2003 challenge — but cannot exit: the terms attach to the works regardless of who holds them.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, public_domain_users, payer,
    organized, generational, trapped, national).

% Diffuse across generations and not yet organized: they will build on today's culture only after terms expire, and each extension postpones that input by decades. They bear the cost without any seat in the process that imposes it — no lobby represented the creators of the 2080s in the 1998 extension debate. Exit is meaningless: they cannot opt out of a term regime that governs works not yet created.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, future_creators, payer,
    powerless, generational, trapped, national).

% Public-interest litigants and the scholars who backed them — the 2003 petitioners, the 2007 Kahle challengers — who spent a decade and substantial resources testing whether 'limited Times' bounds Congress. They lost at every level; after the 2012 decision closed the First Amendment route, the constitutional challenge path is effectively spent. Their resources are consumed and their forum is closed; there is no higher court left to exit to.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_challenge_litigants, payer,
    moderate, biographical, trapped, national).

% Non-agent entity listed for completeness: the fixed boundary content of the Progress Clause's 'limited Times' language. Each upheld extension removes further content from the boundary without formal amendment; what the clause forbids shrinks toward 'not literally forever' as a matter of doctrine. It is this reading's structural victim — the check that pays in erosion — and is excluded from the engine's per-seat arithmetic because it is not an actor.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity, payer,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_fixity).

% The diffuse mass of educators, small publishers, podcasters, remixers, and readers who would use public-domain material but have no organization, no lobby, and no seat in the legislative process that extends terms. They would object that 'limited' is losing meaning; they appear only as amicus signatories and opinion pages after each extension is already law.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, unorganized_public_domain_constituency, excluded,
    powerless, biographical, trapped, national).

% Constitutional law and intellectual-property academics who observe the full structure: the clause's text and history, the extension cycle, the review standard's operation, and the distance between the Framers' fourteen-year-once-renewable design and ninety-five-year terms. They write the critiques, testify, and file briefs; they decide nothing and collect nothing.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__judicial_ambiguity_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__judicial_ambiguity_reading, incumbent_copyright_holders).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__judicial_ambiguity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates interpretive authority over the 'limited Times' boundary between the branches: courts supply a stable, low-cost rule (rational basis deference) that keeps them out of line-drawing the constitutional text gives no metric for, and Congress receives a predictable zone in which to set term policy. Both branches get a settled division of labor; extension legislation gets a stable expectation of validity.
% TRANSFER_FUNCTION: Moves constitutional authority over term length from the judiciary to Congress, and through Congress's process moves public-domain works — and the twenty-year increments of exclusive revenue they represent — from the public and future creators to incumbent holders with each extension cycle.
% ABSENT_VOICES: The diffuse public-domain constituency — future creators, educators, unorganized users — has no seat in the process: extension legislation is written with concentrated industry input, and their objection (that 'limited' is being drained of content) surfaces only as amicus briefs and dissenting opinions after the fact. They are represented vicariously, never present.
% DISAPPEARANCE_RATIONALE: If the deference standard vanished overnight, term-extension challenges would be decided on the merits: courts would either strike retrospective extensions (works flood the public domain, holder portfolios shrink by decades) or articulate a substantive standard with real content — either way the eleven-extension ratchet loses its constitutional insulation, and the cost-benefit of the extension cycle changes for every actor. The legislative-industry machinery would have to renegotiate its relationship with the clause.
% FOUNDING_PROBLEM: The Progress Clause requires copyright terms be 'limited' but supplies no number, no metric, and no maximum; from the first challenges to term legislation, courts faced the problem of reviewing a legislative judgment the text underdetermines. The doctrine was built to solve that: assign the line-drawing to the branch with the political mandate and the fact-finding capacity, and review only for rationality.
% FOUNDING_PROBLEM_CORROBORATION: That the text underdetermines a maximum term is corroborated from outside the beneficiary set: the Eldred dissents (Stevens, Breyer) accept the textual openness while disputing its consequences, and cross-spectrum scholarship concedes the clause gives no metric. What no one outside the benefiting parties attests is that the openness justifies review that has never once invalidated an extension — the dissents, the academic literature, and the historical record of the Framers' fourteen-year-once-renewable design all attest the opposite. Corroboration exists for the founding problem; it is contested for the doctrine's answer to it.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__judicial_ambiguity_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__judicial_ambiguity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__judicial_ambiguity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__judicial_ambiguity_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__judicial_ambiguity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__judicial_ambiguity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.55: the doctrine seizes nothing directly — its take is the check it declines to run. Each upheld extension transfers two decades of works from the public domain to holders, and the transfer compounds (no extension has ever been repealed), so the cumulative take exceeds any single exercise; it stays moderate rather than high because the comity function performed is real and each per-cycle increment is enacted by legislation, not by the doctrine itself. Suppression 0.68: the arrangement's operation IS the suppression of a specific institutional alternative — merits review of term length; after Eldred closed the Progress Clause route and Golan closed the First Amendment route, that alternative is nearly fully suppressed. Suppression here is structural (precedent and justiciability), not internalized, and it is authored as a raw structural property — the engine, not this story, scales extractiveness by directionality and scope. Theater 0.45: rational basis review as performed in this field is substantially ritual — the 'traditional contours' formulation was articulated in Eldred and then not applied against the very act under review — but the arrangement does real work (disposing of challenges, stabilizing legislative expectations), so the performative share is high but short of dominant. Accessibility collapse 0.60: the judicial alternative collapsed almost entirely after 2003-2012; legislative alternatives exist in form (any Congress could shorten terms) but the eleven-extension record shows the process is captured, so alternatives are collapsed in practice rather than in form. Resistance 0.55: sustained and sophisticated — the Eldred litigation campaign, Kahle, Golan, the academic literature, recurring reform bills — but uniformly unsuccessful, which is itself diagnostic of the suppression profile. The three tracked series share one time grid (0 = the 1962 interim-extension era, 14 = the 1976 Act, 36 = the 1998 CTEA, 41 = Eldred, 50 = Golan, 60 = present); suppression_requirement is tracked because the enforcement machinery genuinely hardened at Eldred and Golan rather than merely shifting, and the theater series (0.20 to 0.45) traces the review's drift toward ritual.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats compute differently. From Congress's seat the arrangement is democratic responsiveness plus constitutional comity: the people's representatives set term policy and courts respect it. From the courts' seat it is institutional self-restraint on a question the text underdetermines. From the public-domain users', future creators', and litigants' seats the same arrangement is a closed checkpoint: a review standard that has never invalidated anything, guarding a boundary that has moved in only one direction for sixty years. The engine computes per-seat classifications from the structural data; the divergence between the comity reading and the checkpoint reading is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional_legislature holds the discretion the doctrine insulates and is a declared beneficiary: its derived d sits near the beneficiary end but above pure rentiers, since it also absorbs legitimacy costs when the ratchet draws criticism. Incumbent_copyright_holders are the pure beneficiaries with arbitrage-grade exit (global portfolios, treaty harmonization, lobbying across venues): d nearest the beneficiary end. Federal_courts administer the standard and collect comity — a dual position (agenda_setter with secondary beneficiary) that should compute low-to-moderate d; they carry no base_properties declaration, so the derivation runs on their stakeholder dual role rather than on a beneficiary fact. Public_domain_users, future_creators, and constitutional_challenge_litigants are trapped payers: the terms attach to the works regardless of holder, no forum remains, and d sits near the target end. Constitutional_fixity is named as the reading's structural victim per the expected delta but is authored agent:false — the check that erodes is not an actor, so it is excluded from the d-to-chi derivation; the real-actor victims carry the directionality. Coalition note: the powerless seats attempted exactly the coalition the structure suggests — the Eldred petitioners' alliance of libraries, archivists, and creators — and lost to concentrated industry plus precedent deference; coalition power is available and was insufficient, which is itself structural information about the suppression profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — no judicial metric for 'limited' — is genuinely contested rather than dead: the text really does underdetermine a maximum, so the comity function cannot be dismissed as obsolete. The mandatrophy risk is real in one specific form: if the discretion zone is fully consumed (terms effectively perpetual), the arrangement persists as cover for enclosure while performing only the memory of comity — the theater_ratio series (0.20 rising to 0.45) traces exactly that drift. The tangled_rope classification is what prevents both mislabelings: calling this a rope would hide the asymmetric transfer the extension cycle performs; calling it a snare would deny the genuine institutional-competence problem the Framers' open text created and the real comity value both branches collect. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no automatic capture flag fires on that pairing, but the theater series and the ratchet_reversibility omega are the standing evidence for the transition-to-cover hypothesis, and the scaffold_to_enclosure_hinge omega names the mechanism by which this reading would become the enclosure reading's enabler.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_provenance,
    'This constraint is the judicial_ambiguity_reading of the kernel copyright_constitutional_mandate (''limited Times'' clause); which reading of the clause governs the copyright term regime, and what would the sibling readings (public_scaffold_reading, corporate_enclosure_reading) change structurally?',
    'Comparative structural analysis across the three sibling stories'' beneficiary/victim sets and epsilon referents; the disagreement is located in whether ''limited Times'' is a judicially enforceable boundary, a legislative discretion zone, or a property-right floor.',
    'Under the public_scaffold_reading the same deference computes as a failed check with higher epsilon; under the corporate_enclosure_reading it computes as a proper allocation with lower epsilon; this story''s metrics are valid only for the judicial-ambiguity reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_provenance, conceptual, 'Committer structure: one of three sibling readings of the limited-Times kernel.').

omega_variable(
    ambiguity_genuine_or_constructed,
    'Is the constitutional ambiguity this reading rests on genuine (the text underdetermines a maximum term) or constructed (the Framers'' fourteen-year-once-renewable design and the clause''s stated purpose fix a meaning the doctrine declines to apply)?',
    'Founding-era evidence: the first Copyright Act''s fourteen-plus-fourteen term, the clause''s preamble purpose, contemporaneous usage of ''limited,'' and comparative textual analysis of the Progress Clause against its state antecedents.',
    'If the ambiguity is constructed, the deference is refusal-to-enforce rather than neutral allocation, and epsilon rises toward the enclosure reading''s assessment; if genuine, deference is defensible comity and epsilon stays low-to-moderate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_genuine_or_constructed, empirical, 'Whether the textual ambiguity grounding the deference is real or doctrinally manufactured.').

omega_variable(
    deference_vs_abdication_boundary,
    'Where does deference (respecting a zone of discretion) end and abdication begin, given that rational basis review in this field has never invalidated a single term extension in the doctrine''s history?',
    'Counterfactual and comparative analysis: identify any term extension the doctrine would have struck; compare with rational basis in other fields where legislation occasionally fails; test whether the standard has any falsifiable content.',
    'If the standard has no falsifiable content, the review component is theater and the coordination claim weakens — the arrangement shifts toward extraction riding a comity cover; if some extension would fail, the discretion zone is real and the tangled-rope structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_vs_abdication_boundary, conceptual, 'Whether a review standard that never binds is still review.').

omega_variable(
    ratchet_reversibility,
    'Are copyright term extensions reversible in practice, or does each granted extension become a one-way ratchet (reliance interests, treaty harmonization, retrospective application) that no subsequent Congress or Court will unwind?',
    'Historical test — no extension has ever been repealed; identify the blocking mechanisms (treaty exit costs, reliance doctrine, industry concentration) and model the veto points a hypothetical term-shortening would face.',
    'If irreversible, each exercise of the discretion compounds — the zone functions as an accumulator and effective extraction rises with each cycle even at constant per-cycle epsilon; if reversible, the zone is genuine discretion and extraction stays bounded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratchet_reversibility, empirical, 'Whether the discretion zone is a ratchet or a genuine two-way policy space.').

omega_variable(
    scaffold_to_enclosure_hinge,
    'Does the judicial-ambiguity reading function as the transition mechanism by which the public-scaffold design of copyright (temporary monopoly for a public end) converts into corporate enclosure (effectively perpetual term) without formal constitutional amendment?',
    'Counterfactual constitutional analysis: would the 1998 CTEA have survived a judiciary applying meaningful ''limited Times'' scrutiny, and what term trajectory would have obtained under the public_scaffold_reading''s enforceable-limit frame?',
    'If yes, this reading''s structural role is larger than its direct epsilon suggests — it is the hinge that lets the sibling enclosure reading operate; the network edge to the enclosure story carries the transition, and this story''s classification must be read jointly with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_to_enclosure_hinge, conceptual, 'Whether the deference is the enabling mechanism of the scaffold-to-enclosure transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__judicial_ambiguity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(copy_tr_t0, observed).
narrative_ontology:measurement(copy_tr_t14, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 14, 0.25).
narrative_ontology:measurement_basis(copy_tr_t14, observed).
narrative_ontology:measurement(copy_tr_t36, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement_basis(copy_tr_t36, observed).
narrative_ontology:measurement(copy_tr_t41, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 41, 0.42).
narrative_ontology:measurement_basis(copy_tr_t41, observed).
narrative_ontology:measurement(copy_tr_t50, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement_basis(copy_tr_t50, observed).
narrative_ontology:measurement(copy_tr_t60, copyright_constitutional_mandate__judicial_ambiguity_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement_basis(copy_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(copy_be_t0, observed).
narrative_ontology:measurement(copy_be_t14, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 14, 0.38).
narrative_ontology:measurement_basis(copy_be_t14, observed).
narrative_ontology:measurement(copy_be_t36, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 36, 0.52).
narrative_ontology:measurement_basis(copy_be_t36, observed).
narrative_ontology:measurement(copy_be_t41, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 41, 0.55).
narrative_ontology:measurement_basis(copy_be_t41, observed).
narrative_ontology:measurement(copy_be_t50, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(copy_be_t50, observed).
narrative_ontology:measurement(copy_be_t60, copyright_constitutional_mandate__judicial_ambiguity_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(copy_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(copy_su_t0, observed).
narrative_ontology:measurement(copy_su_t14, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 14, 0.3).
narrative_ontology:measurement_basis(copy_su_t14, observed).
narrative_ontology:measurement(copy_su_t36, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 36, 0.45).
narrative_ontology:measurement_basis(copy_su_t36, observed).
narrative_ontology:measurement(copy_su_t41, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 41, 0.6).
narrative_ontology:measurement_basis(copy_su_t41, observed).
narrative_ontology:measurement(copy_su_t50, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement_basis(copy_su_t50, observed).
narrative_ontology:measurement(copy_su_t60, copyright_constitutional_mandate__judicial_ambiguity_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement_basis(copy_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__judicial_ambiguity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__judicial_ambiguity_reading, copyright_constitutional_mandate__corporate_enclosure_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (the 'limited Times' clause), three readings, three stories — decomposed per the epsilon-invariance principle because the colloquial label 'the constitutional copyright mandate' conflates structurally distinct claims: an enforceable public-good limit (public_scaffold_reading), a legislative discretion zone administered by deferential review (this story), and a property-right floor permitting maximal extension (corporate_enclosure_reading). Each has its own epsilon, beneficiary/victim structure, and classification. The edges run upstream-to-downstream: this reading's deference machinery is what lets the enclosure reading's substantive outcome survive constitutional challenge, and it is the standing obstacle the scaffold reading's enforceable-limit claim must overcome; the sibling stories link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
