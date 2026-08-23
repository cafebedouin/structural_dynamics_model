% ============================================================================
% CONSTRAINT STORY: software_source_status__pragmatic_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__pragmatic_development_reading, []).

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
 *   constraint_id: software_source_status__pragmatic_development_reading
 *   human_readable: Pragmatic Open-Source Methodology Norm (Instrumental-Freedom Reading)
 *   domain: economic/technological/normative
 *
 * SUMMARY:
 *   Since the 1998 OSI reframing, the claim that open development is the
 *   superior methodology has governed the world's software-production
 *   commons: it recruits corporate participation, channels talent toward
 *   public work, and supplies the legitimacy vocabulary of foundations and
 *   procurement. The standing arrangement this story is about — assessed by
 *   this reading's own lights — is the pragmatic-norm-governed commons
 *   economy with permissive licensing defaults: a genuine coordination
 *   achievement (distributed review, reuse, shared infrastructure) riding on
 *   a documented asymmetric transfer (uncompensated maintenance flowing into
 *   commercial products; permissive absorption of commons work into closed
 *   derivatives). The pragmatic reading itself does not treat proprietary
 *   arrangements as injustices — that is the freedom sibling's move — but it
 *   does register commons depletion as a quality threat, which is why its
 *   authored epsilon sits mid-range rather than near zero. KEY AGENTS (by
 *   structural relationship): volunteer_maintainers (primary target: moderate
 *   power, constrained exit — bears the maintenance burden beneath commercial
 *   deployment); corporate_free_riders (primary beneficiary: institutional
 *   power, arbitrage exit); commercial_oss_vendors (beneficiary and
 *   agenda-shaper: institutional power, identity_locked exit);
 *   proprietary_software_firms (opportunistic beneficiary: institutional
 *   power, arbitrage exit); hobbyist_contributors (net-beneficiary workforce:
 *   moderate power, mobile exit); end_users_and_downstream_dependents
 *   (diffuse beneficiary, incident-risk bearer: organized, trapped);
 *   open_source_foundations (agenda setter: institutional, constrained);
 *   copyleft_advocates (excluded voice: moderate, analytical seat);
 *   oss_economists (analytical observer). Claim and metrics are authored
 *   independently: claimed_type=tangled_rope states my structural belief
 *   (both coordination halves and extraction halves are robustly present);
 *   the metrics state what I judge descriptively true; the engine computes
 *   per-seat classifications from the structural data, and any divergence
 *   from my claim is the measurement this corpus exists to take.
 *
 * KEY AGENTS:
 *   - - volunteer_maintainers: Primary target (moderate/constrained) — bears uncompensated maintenance and security response beneath commercial deployments
 *   - - corporate_free_riders: Primary beneficiary (institutional/arbitrage) — consumes the commons without reciprocity obligation, can fork or internalize at will
 *   - - commercial_oss_vendors: Beneficiary + agenda-setter (institutional/identity_locked) — profits from the open posture it also administers; exit would dissolve its self-description
 *   - - proprietary_software_firms: Opportunistic beneficiary (institutional/arbitrage) — embeds permissive commons code in closed products
 *   - - hobbyist_contributors: Net-beneficiary workforce (moderate/mobile) — trades time for skill, reputation, employment
 *   - - end_users_and_downstream_dependents: Diffuse beneficiary, incident-risk bearer (organized/trapped)
 *   - - open_source_foundations: Agenda setter (institutional/constrained) — stewards governance, trademarks, funding
 *   - - copyleft_advocates: Excluded voice (moderate/analytical) — contests the instrumental treatment of freedom
 *   - - oss_economists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, 0.6).
domain_priors:suppression_score(software_source_status__pragmatic_development_reading, 0.37).
domain_priors:theater_ratio(software_source_status__pragmatic_development_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, suppression_requirement, 0.37).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(software_source_status__pragmatic_development_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__pragmatic_development_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__pragmatic_development_reading, "Pragmatic Open-Source Methodology Norm (Instrumental-Freedom Reading)").
narrative_ontology:topic_domain(software_source_status__pragmatic_development_reading, "economic/technological/normative").

domain_priors:requires_active_enforcement(software_source_status__pragmatic_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__pragmatic_development_reading, 'f789838a-4d74-4651-84cb-bf94c4ed7d30').
narrative_ontology:cs_kernel_codification('f789838a-4d74-4651-84cb-bf94c4ed7d30', distributed).
narrative_ontology:cs_authority_grounding('f789838a-4d74-4651-84cb-bf94c4ed7d30', distributed).
narrative_ontology:cs_reading_relation('f789838a-4d74-4651-84cb-bf94c4ed7d30', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('f789838a-4d74-4651-84cb-bf94c4ed7d30', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('f789838a-4d74-4651-84cb-bf94c4ed7d30', software_source_status__utilitarian_hybrid_reading, influences).
narrative_ontology:cs_axiom('f789838a-4d74-4651-84cb-bf94c4ed7d30', foundational, freedom_instrumental_to_quality).
narrative_ontology:cs_axiom_status(freedom_instrumental_to_quality, holdable).
narrative_ontology:cs_axiom_grounding('f789838a-4d74-4651-84cb-bf94c4ed7d30', freedom_instrumental_to_quality, instrumental).
narrative_ontology:cs_axiom('f789838a-4d74-4651-84cb-bf94c4ed7d30', foundational, peer_review_surpasses_internal_review).
narrative_ontology:cs_axiom_status(peer_review_surpasses_internal_review, holdable).
narrative_ontology:cs_axiom_grounding('f789838a-4d74-4651-84cb-bf94c4ed7d30', peer_review_surpasses_internal_review, empirically_contingent).
narrative_ontology:cs_axiom('f789838a-4d74-4651-84cb-bf94c4ed7d30', secondary, permissive_licensing_legitimate).
narrative_ontology:cs_axiom_status(permissive_licensing_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f789838a-4d74-4651-84cb-bf94c4ed7d30', permissive_licensing_legitimate, conventional).
narrative_ontology:cs_reference_frame('f789838a-4d74-4651-84cb-bf94c4ed7d30', instrumental_openness_methodology_standard).
narrative_ontology:cs_drift_state('f789838a-4d74-4651-84cb-bf94c4ed7d30', contemporary_sustainability_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f789838a-4d74-4651-84cb-bf94c4ed7d30', '').
narrative_ontology:cs_kernel_id(software_source_status__pragmatic_development_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, corporate_free_riders).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, commercial_oss_vendors).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, proprietary_software_firms).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, hobbyist_contributors).
narrative_ontology:constraint_beneficiary(software_source_status__pragmatic_development_reading, end_users_and_downstream_dependents).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, volunteer_maintainers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__pragmatic_development_reading, end_users_and_downstream_dependents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain widely-depended-on packages — reviewing pull requests, triaging issue trackers, patching security holes — often as a solo or two-person effort alongside day jobs. Compensation is exceptional rather than normal; after Heartbleed and log4shell a few received short-lived stipends. They can step away, and occasionally do (publishing deprecation notices), but stepping away strands thousands of dependent projects, and for long-tenured stewards the project is bound up with reputation and self-concept in ways that make departure costly well beyond the technical handoff.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, volunteer_maintainers, payer,
    moderate, biographical, constrained, global).

% Operate cloud platforms, devices, and analytics stacks assembled from permissively-licensed components they did not create. Engineering headcount concentrates on integration and differentiation layers; upstream fixes arrive when breakage forces them. Because the licenses impose no reciprocity, improvements can be taken private; because they can hire anyone or fork anything, no single project's health is existential to them.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, corporate_free_riders, beneficiary,
    institutional, generational, arbitrage, global).

% Sell support subscriptions, managed hosting, or enterprise features around an open core, and employ some of the ecosystem's most prolific maintainers. Executives hold foundation board seats and shape governance norms. Their market identity is constituted by openness — reverting to closed distribution, as several did by relicensing, restructures customer relationships and public standing simultaneously, which is why those reversions were fought internally as well as externally.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, commercial_oss_vendors, beneficiary,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, commercial_oss_vendors, agenda_setter).

% Ship closed products embedding open-source libraries wherever the license permits copying without reciprocity. Policy teams resist copyleft mandates and procurement rules that would force disclosure; strategy teams pursue open approaches opportunistically when developer mindshare or acquisition pipelines favor them. Nothing in the norm's current form obliges them to return code.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, proprietary_software_firms, beneficiary,
    institutional, generational, arbitrage, global).

% Submit patches, documentation, and translations to projects they use, trading evenings-and-weekends time for skill growth, portfolio evidence, and community standing. Most hold salaried jobs elsewhere; public contribution visibility converts into job offers. They can stop contributing at any time with little structural consequence to themselves.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, hobbyist_contributors, beneficiary,
    moderate, biographical, mobile, global).

% Run banks, hospitals, factories, and consumer applications on dependency trees they cannot see, built by people they will never meet. They receive low-cost, high-quality components and carry concentrated incident risk when a dependency fails — log4shell reached them within hours. Swapping foundational components mid-operation is rarely feasible; their leverage is indirect, exercised through procurement and regulators rather than through the commons itself.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, end_users_and_downstream_dependents, beneficiary,
    organized, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__pragmatic_development_reading, end_users_and_downstream_dependents, payer).

% Hold trademarks, run security-response processes, administer grant programs, and host technical committees for the major projects. Corporate members fund them and fill working groups; balancing member interests against project health is their daily negotiation. Repositioning or dissolving a foundation strands the projects it shelters, so course corrections are slow.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Argue that treating software freedom as a means rather than an end trades away what made the commons worth defending, and that permissive defaults invite enclosure. They maintain GPL-family counterweights and publish critiques of corporate open source; as industry venues consolidate around pragmatic vocabulary, they find fewer platforms willing to host their framing.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, copyleft_advocates, excluded,
    moderate, civilizational, analytical, global).

% Measure contribution concentration, funding flows, defect densities, and license-change waves across the ecosystem; advise public funders and foundations; publish the censuses that turn anecdotes about burned-out maintainers into audited numbers.
narrative_ontology:constraint_stakeholder(software_source_status__pragmatic_development_reading, oss_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__pragmatic_development_reading, corporate_free_riders).
narrative_ontology:fixing_cost_class(software_source_status__pragmatic_development_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production, review, and hardening of software beyond any single organization's capacity: strangers pool code through transparent repositories, defects surface through distributed review, and reusable components spare every downstream team from rebuilding cryptography, networking, compression, and identity handling from scratch.
% TRANSFER_FUNCTION: Moves skilled maintenance and review labor from volunteers and mission-driven contributors into the products and platforms of the firms that ship them; moves reputation, career capital, and occasional grants back toward contributors; and moves permissively-licensed code from the shared commons into proprietary derivatives whenever a downstream firm chooses to close it.
% ABSENT_VOICES: Copyleft advocates who regard instrumental freedom as a betrayal speak at the margins of industry venues; maintainers of critical single-person dependencies hold no seat in the procurement conversations that determine whether their work is funded; and end organizations consuming the dependency tree have no voice in license or governance decisions until an incident reaches them.
% DISAPPEARANCE_RATIONALE: Foundations, governance processes, funding pipelines, hiring rubrics, and two decades of tooling are organized around the expectation that openness is the default engineering choice. Overnight removal would force every firm to re-legitimate its sourcing under either a freedom-first ethic (copyleft reciprocity reshaping corporate participation) or a property-first regime (access renegotiated contractually), rerouting contribution incentives, procurement, and the careers built on public portfolios.
% FOUNDING_PROBLEM: In 1998 the collaborative-development model demonstrably worked, but its framing as an ethical freedom movement kept commercial firms at arm's length; the pragmatic program reframed openness as engineering method — cheaper defect discovery, faster innovation, credible infrastructure — so enterprises could adopt it without endorsing a moral doctrine.
% FOUNDING_PROBLEM_CORROBORATION: Software-engineering research (large-scale defect-density and review studies) and post-incident security advisories from state agencies corroborate the methodological claims from outside the beneficiary firms; the freedom-imperative camp provides adverse corroboration, documenting what the instrumental settlement conceded; corporate open-source offices attest continued necessity but sit inside the benefiting set, so their testimony weighs accordingly.
narrative_ontology:disappearance_verdict(software_source_status__pragmatic_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__pragmatic_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__pragmatic_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__pragmatic_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__pragmatic_development_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__pragmatic_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__pragmatic_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__pragmatic_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.60: the transfer asymmetry is large and grew over the interval (single maintainers supporting infrastructure deployed by trillion-dollar firms; permissive absorption into closed derivatives), yet by this reading's own lights the arrangement is not fundamentally illegitimate — proprietary code is not injustice, and the commons still delivers a surplus no alternative has matched. That combination — real, growing, identifiable transfer atop a genuine, massive coordination function — is the tangled_rope structure. Suppression 0.37 and unscaled: no legal coercion anywhere; the compelling forces are reputational (closed development stigmatized in hiring and conference economies), governance gatekeeping, and the career premium on visible open work; alternatives (proprietary, source-available, strong copyleft, hybrid) remain fully legal and commonly practiced. Suppression_requirement series tracks enforcement-capacity buildup, not fee extraction: informal 1998-era community ethos professionalized into foundation governance, code-of-conduct regimes, CLA/DCO machinery, and security-response processes — enforcement infrastructure matured and hardened, hence the rising series. Theater_ratio 0.34: open-washing (badges, performative hacktoberfest merges, community-flavored marketing) is real and rising with the marketing value of openness, but the majority of activity remains functionally merged code and reviewed fixes. Accessibility_collapse 0.30: understanding the norm does not foreclose alternatives — license-retreat waves prove exit is practicable. Resistance 0.50: SSPL/BUSL relicensings, sustainability campaigns, maintainer resignation notices, and EU CRA friction constitute sustained pushback from inside the arrangement. Coordination type identity_coordination: the dominant function is recruiting and retaining a distributed workforce through reputation, membership, and recognition economies — if recognition failed, contribution would collapse before any technical standard did. The known FNL gaming risk with identity framing (relational cover for extraction) is acknowledged; the conservative floor and the audited contribution asymmetry are the counters. Measurement series share one grid ({0,4,9,13,18,23,27}) across all three tracked metrics.
 *
 * PERSPECTIVAL GAP:
 *   Three institutional-power seats experience structurally different arrangements despite nominal parity. The free-rider seat, holding arbitrage exit, experiences the norm as windfall infrastructure — commitment stays optional, so the arrangement computes as near-pure subsidy from its position. The vendor seat is identity_locked at the organizational level: the firm has become its open posture; leaving it would dissolve the business's self-description along with its revenue, so the same norm computes as constitutive stewardship with real contribution obligations attached. The proprietary firm experiences selective convenience — consuming where licenses permit, resisting where mandates loom. The payer seat (maintainers) computes the highest burden: moderate power, constrained exit, identity binding between steward and project keep labor supplied below replacement cost. The engine derives these per-seat classifications from power, exit, and role data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation and no overrides were needed: volunteer_maintainers (declared victim, moderate power, constrained exit) sit near the full-target end; corporate_free_riders (declared beneficiary, arbitrage exit) sit nearest the beneficiary pole — arbitrage-grade exit pushes them furthest toward subsidy; commercial_oss_vendors (beneficiary + agenda_setter, identity_locked exit) derive low-but-not-floor d, the identity lock tempering the mobility advantage other beneficiaries enjoy; proprietary_software_firms (beneficiary, arbitrage) derive low d; hobbyist_contributors derive mildly favorable d — career-capital returns genuinely offset time cost for most; end_users_and_downstream_dependents derive near-zero d with trapped exit amplifying their exposure to incident costs rather than to fees; foundations derive modest administrative d; copyleft_advocates derive approximately symmetric d as a discursive critic that neither collects nor pays materially. Intra-power-atom differentiation (three institutional beneficiaries with distinct relationships) is carried by exit_options, secondary roles, and scope rather than by directionality_overrides, which the schema keys to power atoms and would therefore flatten these distinctions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making collaborative development commercially legible — was substantially accomplished within roughly a decade; what the arrangement now does (steering sustainability, reciprocity, and security of the commons it built) is a newer mandate whose liveness the parties dispute. Recording founding_problem_status=contested rather than dead keeps the mismatch consumer honest: a dead-status + world_rearranges pairing would flag capture/zombie, which is not yet the right verdict while theater_ratio sits at 0.34, coordination function is intact, and administrators retain change-capacity they have begun to exercise (public funding schemes, foundation sustainability programs). Mandatrophy discipline cuts both ways here: a pure-rope reading would erase the maintainer burden that license-retreat waves made undeniable; a pure-snare reading would erase the surplus that funds the largest voluntary production system in history. The tangled_rope claim preserves both halves; the temporal series shows the extraction component accumulating slowly (0.38 to 0.60) rather than the constraint decaying into performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is one of four readings of the software_source_status kernel — how would each sibling reading re-author the same terrain?',
    'Compile and classify the three sibling stories (freedom_imperative_reading, property_rights_reading, utilitarian_hybrid_reading) and compare epsilon, victim sets, and computed types against this file.',
    'The freedom reading would expand the victim set to every user of non-free code and raise epsilon sharply over proprietary arrangements; the property reading would relocate the violation to uncompensated copying of protected works; the hybrid reading would compress epsilon and regionalize blame to context mismatches. The divergence pattern localizes the kernel dispute to the victim-set boundary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: this story is the pragmatic reading among four declared readings of one kernel.').

omega_variable(
    norm_vs_material_commons_boundary,
    'Is the constraint under classification the normative standard (openness-as-superior-methodology) or the material commons economy that standard governs?',
    'Decompose: author the permissive-absorption channel (commons code taken into closed derivatives) as its own family member and classify it separately; wide epsilon divergence between sub-story and parent indicates the parent''s referent was conflating norm and material base.',
    'Successful decomposition would isolate the absorption dynamic as a higher-epsilon constraint linked via network.affects_constraints, lowering this story''s residual epsilon and sharpening the family structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_vs_material_commons_boundary, conceptual, 'Referent boundary: normative standard versus governed material economy.').

omega_variable(
    uncompensated_maintenance_share,
    'What fraction of critical-infrastructure maintenance value is currently uncompensated?',
    'Funding-flow audits reconciling CHAOSS contribution metrics, Sovereign Tech Fund and foundation disbursements, and maintainer censuses against estimated engineering hours consumed downstream.',
    'A higher uncompensated share raises effective extraction at the maintainer seat and strengthens per-seat drift toward snare-side classifications; a falling share would support a rope-ward correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(uncompensated_maintenance_share, empirical, 'Magnitude of the labor-transfer asymmetry at the payer seat.').

omega_variable(
    permissive_guarding_causality,
    'Does permissive licensing — the element distinctive to this reading versus the freedom-imperative sibling — causally increase proprietary absorption of commons work?',
    'Matched comparison of absorption and re-closure rates between MIT/BSD-licensed and strong-copyleft projects of similar popularity, age, and dependency centrality.',
    'If permissive projects are absorbed faster, this reading''s licensing tolerance is the load-bearing enabler of the transfer asymmetry — sharpening the structural contrast with the freedom sibling and raising the weight of the licensing clause in classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_guarding_causality, empirical, 'Whether this reading''s permissive default causes the enclosure it merely tolerates.').

omega_variable(
    open_washing_measurability,
    'Where is the theater/functional boundary in corporate open-source activity?',
    'Sampled audits of corporate contributions scoring merged-versus-promotional ratio, maintainer tenure, and upstream acceptance rates.',
    'Higher measured theater would advance the piton-drift window; lower measured theater would confirm functional dominance and stabilize the tangled_rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_washing_measurability, empirical, 'Measurability of performative versus functional open-source participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__pragmatic_development_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sssource_pragmatic_tr_t0, software_source_status__pragmatic_development_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(sssource_pragmatic_tr_t4, software_source_status__pragmatic_development_reading, theater_ratio, 4, 0.19).
narrative_ontology:measurement(sssource_pragmatic_tr_t9, software_source_status__pragmatic_development_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement(sssource_pragmatic_tr_t13, software_source_status__pragmatic_development_reading, theater_ratio, 13, 0.29).
narrative_ontology:measurement(sssource_pragmatic_tr_t18, software_source_status__pragmatic_development_reading, theater_ratio, 18, 0.31).
narrative_ontology:measurement(sssource_pragmatic_tr_t23, software_source_status__pragmatic_development_reading, theater_ratio, 23, 0.33).
narrative_ontology:measurement(sssource_pragmatic_tr_t27, software_source_status__pragmatic_development_reading, theater_ratio, 27, 0.34).

% Extraction over time
narrative_ontology:measurement(sssource_pragmatic_be_t0, software_source_status__pragmatic_development_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sssource_pragmatic_be_t4, software_source_status__pragmatic_development_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(sssource_pragmatic_be_t9, software_source_status__pragmatic_development_reading, base_extractiveness, 9, 0.47).
narrative_ontology:measurement(sssource_pragmatic_be_t13, software_source_status__pragmatic_development_reading, base_extractiveness, 13, 0.51).
narrative_ontology:measurement(sssource_pragmatic_be_t18, software_source_status__pragmatic_development_reading, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(sssource_pragmatic_be_t23, software_source_status__pragmatic_development_reading, base_extractiveness, 23, 0.57).
narrative_ontology:measurement(sssource_pragmatic_be_t27, software_source_status__pragmatic_development_reading, base_extractiveness, 27, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(sssource_pragmatic_su_t0, software_source_status__pragmatic_development_reading, suppression_requirement, 0, 0.21).
narrative_ontology:measurement(sssource_pragmatic_su_t4, software_source_status__pragmatic_development_reading, suppression_requirement, 4, 0.23).
narrative_ontology:measurement(sssource_pragmatic_su_t9, software_source_status__pragmatic_development_reading, suppression_requirement, 9, 0.26).
narrative_ontology:measurement(sssource_pragmatic_su_t13, software_source_status__pragmatic_development_reading, suppression_requirement, 13, 0.29).
narrative_ontology:measurement(sssource_pragmatic_su_t18, software_source_status__pragmatic_development_reading, suppression_requirement, 18, 0.32).
narrative_ontology:measurement(sssource_pragmatic_su_t23, software_source_status__pragmatic_development_reading, suppression_requirement, 23, 0.35).
narrative_ontology:measurement(sssource_pragmatic_su_t27, software_source_status__pragmatic_development_reading, suppression_requirement, 27, 0.37).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__pragmatic_development_reading, identity_coordination).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__property_rights_reading).
narrative_ontology:affects_constraint(software_source_status__pragmatic_development_reading, software_source_status__utilitarian_hybrid_reading).

% DUAL FORMULATION NOTE:
% Colloquially 'the open source versus proprietary debate' is one argument; structurally it is four constraints with distinct epsilon referents and victim boundaries (see commentary.kernel_context). Family structure: this pragmatic reading stands in premise-conflict with the freedom-imperative sibling, coexists with the property-rights sibling, and exerts downstream structural influence on the hybrid sibling, whose licensing-pluralism settlement became practicable only after this reading's institutional victory normalized plural licensing. Each file links the others via affects_constraints; license-regime shocks (SSPL/BUSL waves, AI-training appropriation disputes) should propagate across the family edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
